#!/usr/bin/env python3
"""Diagnostic pulse-dictionary fit, not a recovered LSDJ score or clean stems."""
import csv
import json
from pathlib import Path
import numpy as np
from scipy import signal
from scipy.io import wavfile
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[2]
WORK = ROOT/'scratchpad/audio/courtesy'
SR, N, HOP = 22050, 4096, 220


def dictionary():
    window = signal.windows.hann(N, sym=False)
    definitions, columns = [], []
    for midi in range(36, 85):
        hz = 440*2**((midi-69)/12)
        for duty in [.125, .25, .5]:
            # Band-limited Fourier pulse: omit harmonics beyond Nyquist.
            t = np.arange(N)/SR
            wave = np.zeros(N)
            for harmonic in range(1, int((SR/2-1)//hz)+1):
                coefficient = 2*np.sin(np.pi*harmonic*duty)/(np.pi*harmonic)
                wave += coefficient*np.cos(2*np.pi*harmonic*hz*t-np.pi*harmonic*duty)
            spectrum = np.abs(np.fft.rfft(wave*window))/window.sum()
            definitions.append((midi, duty))
            columns.append(spectrum)
    return definitions, np.asarray(columns).T


def fit(magnitude, raw):
    # Weight bins to reduce low-frequency dominance without whitening silence.
    weight = 1/np.sqrt(np.maximum(magnitude.mean(axis=1), .0005))
    basis = raw*weight[:, None]
    norms = np.linalg.norm(basis, axis=0)
    basis /= norms
    remainder = magnitude*weight[:, None]
    activation = np.zeros((raw.shape[1], magnitude.shape[1]))
    # Two pulse hypotheses per frame, not four automatically separated voices.
    for _ in range(2):
        correlations = basis.T @ remainder
        winner = np.argmax(correlations, axis=0)
        amount = np.maximum(correlations[winner, np.arange(len(winner))], 0)
        activation[winner, np.arange(len(winner))] += amount/norms[winner]
        remainder -= basis[:, winner]*amount
    return activation


def main():
    sr, stereo = wavfile.read(WORK/'decoded.wav')
    mono = stereo[:20*sr].mean(axis=1)
    x = signal.resample_poly(mono, SR, sr)
    freq, times, z = signal.stft(x, SR, nperseg=N, noverlap=N-HOP)
    magnitude = np.abs(z)
    definitions, raw = dictionary()
    activation = fit(magnitude, raw)
    predicted = raw@activation
    # Mixture phase is useful for an auditable spectral mask, not clean synthesis.
    mask = np.minimum(predicted/np.maximum(magnitude, 1e-10), 1)
    _, estimated = signal.istft(z*mask, SR, nperseg=N, noverlap=N-HOP)
    estimated = estimated[:len(x)]
    residual = x-estimated
    wavfile.write(WORK/'pulse_mask_estimate.wav', SR, estimated.astype(np.float32))
    wavfile.write(WORK/'pulse_mask_residual.wav', SR, residual.astype(np.float32))
    # Independent phase-continuous pulse synthesis exposes timbre/pitch mistakes.
    synth = np.zeros(len(x))
    t = np.arange(len(x))/SR
    events = []
    for k, (midi, duty) in enumerate(definitions):
        envelope = signal.medfilt(activation[k], 5)
        if np.max(envelope) < .015:
            continue
        active = envelope > max(.015, float(np.max(envelope))*.15)
        edges = np.diff(np.r_[False, active, False].astype(int))
        for start, end in zip(np.flatnonzero(edges == 1), np.flatnonzero(edges == -1)):
            if (end-start)*HOP/SR >= .06:
                events.append({'start_seconds': float(times[start]),
                               'end_seconds': min(20., float(times[end-1]+HOP/SR)),
                               'midi_candidate': midi, 'duty_candidate': duty,
                               'status': 'unreviewed spectral hypothesis; channel unknown'})
        hz = 440*2**((midi-69)/12)
        wave = np.zeros(len(x))
        for h in range(1, int((SR/2-1)//hz)+1):
            wave += 2*np.sin(np.pi*h*duty)/(np.pi*h)*np.cos(2*np.pi*h*hz*t-np.pi*h*duty)
        synth += wave*np.interp(t, times, envelope)
    # Preserve relative gains; apply only a documented anti-clipping gain.
    gain = min(1., .95/max(np.max(np.abs(synth)), 1e-9))
    wavfile.write(WORK/'pulse_resynthesis.wav', SR, (synth*gain).astype(np.float32))
    events.sort(key=lambda e:e['start_seconds'])
    (ROOT/'soundtrack/courtesy.pulse_candidates.json').write_text(json.dumps({
        'interval_seconds': [0,20], 'method':'two-term greedy pulse spectral fit',
        'limitations':['equal-tempered fixed pitches only', 'no detune, sweeps, wave or noise model',
                       'duty and octave ambiguous in a mixed recording',
                       'events are not a transcription; no hardware channel identities'],
        'synthesis_gain': float(gain), 'events':events}, indent=2)+'\n')
    fig, axes = plt.subplots(3,1,figsize=(14,10),constrained_layout=True)
    for ax, data, title in zip(axes, [magnitude,predicted,np.abs(z)*(1-mask)],
                              ['Original opening','Two-pulse spectral prediction','Unexplained spectral residual']):
        ax.pcolormesh(times, freq, 20*np.log10(np.maximum(data,1e-7)),
                      shading='auto', vmin=-80,vmax=-20,rasterized=True)
        ax.set(yscale='log',ylim=(50,8000),xlim=(0,20),ylabel='Hz',title=title)
    axes[-1].set_xlabel('seconds from decoded start')
    fig.savefig(WORK/'pulse_fit.png',dpi=120)
    # Test fitting on an independent synthetic single-pulse example.
    k=definitions.index((57,.25))
    test_t = np.arange(N)/SR
    test_wave = .15*(signal.square(2*np.pi*220*test_t + .73, duty=.25)+.5)
    window = signal.windows.hann(N, sym=False)
    synthetic = (np.abs(np.fft.rfft(test_wave*window))/window.sum())[:,None]
    check=fit(synthetic,raw)
    assert np.argmax(check[:,0])==k, 'synthetic pitch/duty recovery failed'
    assert abs(check[k,0]-.3)<.03, 'synthetic gain recovery failed'
    assert np.max(np.abs(x-(estimated+residual)))<1e-6
    assert np.isfinite(synth).all()
    print(json.dumps({'candidate_events':len(events),'residual_rms_ratio':float(np.linalg.norm(residual)/np.linalg.norm(x)),
                      'validation':'synthetic dictionary recovery and residual reconstruction pass'},indent=2))

if __name__=='__main__':
    main()

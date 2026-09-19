#!/usr/bin/env python3
"""Reproducible first-pass soundtrack analysis; estimates require listening review.
Run with venv/bin/python tools/audio/analyse.py from the repository root.
"""
import hashlib
import json
from pathlib import Path
import subprocess

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from scipy import signal
from scipy.io import wavfile

ROOT = Path(__file__).resolve().parents[2]
RATE = 22050
HOP = 220


def command(args):
    return subprocess.check_output(args, text=True)


def analyse(path):
    name = path.name.split(' - ', 1)[1].split(' [', 1)[0].lower()
    work = ROOT / 'scratchpad/audio' / name
    work.mkdir(parents=True, exist_ok=True)
    pcm = work / 'decoded.wav'
    probe = json.loads(command(['ffprobe', '-v', 'quiet', '-show_format',
                                '-show_streams', '-of', 'json', str(path)]))
    # Preserve decoded stereo origin; no silence trimming or normalization.
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', str(path), '-vn',
                    '-c:a', 'pcm_f32le', str(pcm)], check=True)
    sr, stereo = wavfile.read(pcm)
    mono = stereo.mean(axis=1)
    x = signal.resample_poly(mono, RATE, sr)
    freq, times, z = signal.stft(x, RATE, nperseg=2048,
                                noverlap=2048-HOP, boundary=None)
    mag = np.abs(z)
    # Whiten frequency bins so bass energy does not overwhelm higher attacks.
    whitened = np.log1p(mag / np.maximum(np.median(mag, axis=1)[:, None], 1e-5))
    bands = [(40, 180), (180, 2000), (2000, 9000)]
    flux = []
    for low, high in bands:
        b = whitened[(freq >= low) & (freq < high)]
        f = np.maximum(np.diff(b, axis=1, prepend=b[:, :1]), 0).mean(axis=0)
        f /= max(float(np.percentile(f, 95)), 1e-9)
        flux.append(f)
    onset = np.mean(flux, axis=0)
    onset = np.maximum(onset - signal.medfilt(onset, 101), 0)
    dt = HOP / RATE
    ac = signal.correlate(onset, onset, mode='full', method='fft')[len(onset)-1:]
    ac /= np.arange(len(ac), 0, -1)
    lags = np.arange(len(ac))
    candidates, _ = signal.find_peaks(ac)
    candidates = [int(i) for i in candidates if 60/200 <= i*dt <= 60/65]
    candidates.sort(key=lambda i: ac[i], reverse=True)
    tempos = [{'bpm': round(60/(i*dt), 3), 'periodicity': float(ac[i]/ac[0])}
              for i in candidates[:8]]
    # Refine regular-grid tempo/phase over the whole recording. This deliberately
    # does not assert meter/downbeats or support variable tempo automatically.
    best = None
    for coarse in tempos[:4]:
        for bpm in np.arange(coarse['bpm']-2, coarse['bpm']+2, .02):
            period = 60/bpm
            phase = np.linspace(0, period, 64, endpoint=False)
            grid = phase[:, None] + np.arange(0, times[-1], period)[None, :]
            values = np.interp(grid, times, onset, left=0, right=0)
            scores = values.mean(axis=1)
            k = int(np.argmax(scores))
            if best is None or scores[k] > best[0]:
                best = (float(scores[k]), float(bpm), float(phase[k]))
    _, bpm, phase = best
    beats = np.arange(phase, len(mono)/sr, 60/bpm)
    np.savetxt(work/'candidate_beats.csv', beats, header='seconds_from_decoded_start', comments='')
    # Click overlay makes half/double tempo and accumulated drift audible.
    preview = stereo.copy() * .65
    n = int(sr*.025)
    click = .16*np.sin(2*np.pi*1500*np.arange(n)/sr)*np.exp(-np.arange(n)/(sr*.005))
    for beat in beats:
        at = round(beat*sr)
        count = min(n, len(preview)-at)
        preview[at:at+count] += click[:count, None]
    wavfile.write(work/'candidate_click.wav', sr, np.clip(preview, -1, 1).astype(np.float32))
    # Coarse chroma and energy descriptors find repetitions; not stem separation.
    chroma = np.zeros((12, mag.shape[1]))
    bins = np.flatnonzero((freq >= 65) & (freq <= 4000))
    pitch = np.round(69+12*np.log2(freq[bins]/440)).astype(int)%12
    for pc in range(12):
        chroma[pc] = mag[bins[pitch == pc]].sum(axis=0)
    block = max(1, round(1/dt))
    length = chroma.shape[1]//block
    features = chroma[:, :length*block].reshape(12, length, block).mean(axis=2)
    features /= np.maximum(np.linalg.norm(features, axis=0), 1e-9)
    similarity = features.T @ features
    fig, ax = plt.subplots(4, 1, figsize=(14, 12), constrained_layout=True)
    stride = max(1, sr//200)
    ax[0].plot(np.arange(0, len(mono), stride)/sr, stereo[::stride], linewidth=.35)
    ax[0].set(title=f'{name}: stereo waveform (untrimmed)', ylabel='amplitude')
    ax[1].pcolormesh(times, freq, 20*np.log10(np.maximum(mag, 1e-7)),
                      vmin=-85, vmax=-20, shading='auto', rasterized=True)
    ax[1].set(yscale='log', ylim=(40, 10000), ylabel='Hz')
    ax[2].plot(times, onset, linewidth=.5)
    ax[2].set(title=f'Unreviewed constant beat-grid candidate: {bpm:.2f} BPM', xlabel='seconds')
    ax[3].imshow(similarity, origin='lower', aspect='auto',
                  extent=(0, length*block*dt, 0, length*block*dt), vmin=0, vmax=1)
    ax[3].set(title='Coarse chroma similarity (not proof of identical phrases)', xlabel='seconds', ylabel='seconds')
    fig.savefig(work/'overview.png', dpi=120)
    plt.close(fig)
    # Fixed excerpts provide sparse/dense choices for subsequent human review.
    excerpts = []
    for start in [0, 30, 60, 90, 120]:
        end = min(start+20, len(mono)/sr)
        if end <= start:
            continue
        wavfile.write(work/f'excerpt_{start:03d}.wav', sr, stereo[round(start*sr):round(end*sr)])
        excerpts.append({'start_seconds': start, 'end_seconds': end, 'reviewed': False})
    result = {
        'source': str(path.relative_to(ROOT)), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
        'container_duration_seconds': float(probe['format']['duration']),
        'decoded_sample_rate': sr, 'decoded_samples': len(stereo),
        'decoded_duration_seconds': len(stereo)/sr,
        'time_origin': 'first decoded PCM sample; ffmpeg applies available decoder skip metadata; no manual trimming',
        'stereo_correlation': float(np.corrcoef(stereo.T)[0, 1]),
        'peak_amplitude': float(np.max(np.abs(stereo))),
        'tempo_candidates': tempos,
        'constant_grid_hypothesis': {'bpm': bpm, 'phase_seconds': phase, 'reviewed': False,
                                     'downbeat_known': False, 'variable_tempo_checked': False},
        'excerpts': excerpts,
        'generated_directory': str(work.relative_to(ROOT)),
        'status': 'automatic analysis only; no verified beat map or transcription',
    }
    (ROOT/'soundtrack'/f'{name}.analysis.json').write_text(json.dumps(result, indent=2)+'\n')
    print(name, f'{len(stereo)/sr:.3f}s', f'candidate {bpm:.2f} BPM', flush=True)


if __name__ == '__main__':
    (ROOT/'soundtrack').mkdir(exist_ok=True)
    versions = {'ffmpeg': command(['ffmpeg', '-version']).splitlines()[0],
                'numpy': np.__version__, 'analysis_rate': RATE, 'hop': HOP}
    import scipy
    versions['scipy'] = scipy.__version__
    (ROOT/'soundtrack/tools.json').write_text(json.dumps(versions, indent=2)+'\n')
    for source in sorted((ROOT/'assets').glob('*.mp3')):
        analyse(source)

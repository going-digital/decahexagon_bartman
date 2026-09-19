#!/usr/bin/env python3
"""Compare time resolutions without asserting that harmonic peaks are notes."""
from pathlib import Path
import numpy as np
from scipy import signal
from scipy.io import wavfile
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[2]
RATE = 22050
HOP = 55  # 2.49 ms; hop is not the window's resolving power.


def plot(x, path, title, start=0):
    fig, axes = plt.subplots(3, 1, figsize=(16, 10), constrained_layout=True)
    for ax, size in zip(axes, [4096, 1024, 512]):
        f, t, z = signal.stft(x, RATE, nperseg=size, noverlap=size-HOP,
                              boundary=None)
        ax.pcolormesh(t+start, f, 20*np.log10(np.maximum(np.abs(z), 1e-7)),
                      vmin=-75, vmax=-20, shading='auto', rasterized=True)
        ax.set(ylim=(100, 5000), ylabel='Hz',
               title=f'{size/RATE*1000:.1f} ms window; {RATE/size:.1f} Hz bin spacing')
    axes[-1].set_xlabel('seconds from decoded start')
    fig.suptitle(title)
    fig.savefig(path, dpi=130)
    plt.close(fig)


def main():
    for name in ['courtesy', 'focus', 'otis']:
        work = ROOT/'scratchpad/audio'/name
        sr, stereo = wavfile.read(work/'decoded.wav')
        for start in [0, 15]:
            mono = stereo[start*sr:(start+4)*sr].mean(axis=1)
            x = signal.resample_poly(mono, RATE, sr)
            plot(x, work/f'arpeggio_resolution_{start:03d}.png',
                 f'{name}: resolution comparison, not a note transcription', start)
    # Known synthetic fast arpeggio demonstrates how the previous window blurs
    # sequential pitches. Phase is continuous and no extra voices are present.
    t = np.arange(RATE*2)/RATE
    notes = np.array([60, 64, 67, 72])[(t/.02).astype(int)%4]
    frequency = 440*2**((notes-69)/12)
    phase = np.cumsum(frequency)/RATE
    x = .18*np.sin(2*np.pi*phase)
    plot(x, ROOT/'scratchpad/audio/arpeggio_control.png',
         'Known single voice: C4 E4 G4 C5, one pitch every 20 ms')
    print('Generated six recording comparisons and one known-arpeggio control')


if __name__ == '__main__':
    main()

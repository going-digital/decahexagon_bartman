# Soundtrack analysis progress

First analysis batch: all three recordings have been decoded without manual
trimming, hashed, and analysed. Per-track JSON files retain provenance, tempo
hypotheses, stereo measurements and unreviewed excerpt intervals.

| Track | Regular-grid candidate | Stereo correlation |
| --- | ---: | ---: |
| Courtesy | 130.03 BPM | 0.912 |
| Focus | 86.99 BPM | 0.967 |
| Otis | 67.51 BPM | 0.919 |

These are automatic periodicity estimates, not verified musical tempos.
Half/double-time alternatives must be auditioned (particularly Focus and Otis).
The current estimator fits a constant grid; it does not establish downbeats,
swing or tempo changes. Spectral-window timing can bias inferred attack phase.
No separated channels, instrument models or note transcriptions exist yet.

## Reproduce

From the repository root, with ffmpeg/ffprobe on PATH and numpy, scipy and
matplotlib installed in the existing venv:

```sh
MPLCONFIGDIR=/private/tmp/hexagon-matplotlib venv/bin/python tools/audio/analyse.py
```

Tracked outputs are compact JSON summaries. Large generated outputs live under
`scratchpad/audio/<track>/`:

- decoded.wav: float PCM reference retaining stereo and original sample rate.
- candidate_beats.csv: estimated beat times relative to decoded sample zero.
- candidate_click.wav: reference attenuated with a click on that candidate grid.
- overview.png: waveform, spectrogram, onset curve and coarse chroma similarity.
- excerpt_000.wav through excerpt_120.wav: five 20-second listening candidates.

The coarse chroma plot indicates spectral resemblance, not identical phrases;
shared harmonics and broadband percussion can produce high similarity. It must
not be used as an automatic phrase deduplication decision.

## Next work

1. Audition the click overlays at several points, explicitly testing half/double
   tempo and checking phase drift. Establish downbeat and phrase annotations.
2. Start instrument analysis with Courtesy's opening 0–20 seconds: the inspected
   overview shows a quieter opening followed by denser material, giving a useful
   provisional comparison. This selection is visual and remains subject to
   listening review. Inspect smaller spectrogram windows before fitting notes.
3. Fit pulse/harmonic candidates in those windows and render them, retaining
   unexplained residuals instead of forcing four clean stems.
4. Review that reconstruction before building the sample/event playback excerpt.

PC soundtrack edit correspondence and track-to-level mapping remain unverified.
No Amiga runtime code changed in this batch, so no new emulator playback check
is claimed. The first FS-UAE audio check belongs to the reconstructed excerpt.

Validation: all three decoded/reference-click pairs have matching sample rates
and frame counts, finite samples, strictly increasing in-range beat candidates,
five excerpts and an overview image. Focus's float decode peaks above full
scale (1.087); the float reference preserves it. Click previews attenuate the
source before adding clicks; any later integer export must choose gain explicitly.

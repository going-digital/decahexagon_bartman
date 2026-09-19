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
At this first-batch stage no separated channels, instrument models or note
transcriptions existed; see the second-batch progress below.

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

## Second batch: timing diagnostics and first pulse fit

Run `venv/bin/python tools/audio/check_grid.py` to regenerate
`grid_diagnostics.json`. It measures local onset phase in 20-second windows for
half/base/double-tempo alternatives. These measurements do not resolve meter.
Courtesy's strongest phase switches by roughly half a 130-BPM beat after the
opening, then stays near that phase. Focus also switches by approximately half
its candidate beat. Otis has quarter/half-cycle alternatives. This is evidence
that the initial phase/metrical choices are ambiguous, not proof of tempo changes.
The analysis has not established a reviewed full-song beat map.

Run the following for the first Courtesy opening model:

```sh
OPENBLAS_NUM_THREADS=1 MPLCONFIGDIR=/private/tmp/hexagon-matplotlib venv/bin/python tools/audio/fit_opening.py
```

This fits up to two pulse spectra per frame over 0–20 seconds, using MIDI pitches
36–84 and three duty hypotheses. The model assumes fixed equal-tempered pitches;
it does not yet represent sweeps, detuning, evolving waves or noise. Its 160
candidate segments in `courtesy.pulse_candidates.json` are unreviewed spectral
hypotheses, not a score or identified Game Boy channels. Greedy fits can select
harmonics and fragment notes; independent envelope smoothing may also overlap
candidates beyond two voices. They must not be exported straight to Paula.

Generated files under `scratchpad/audio/courtesy/`:

- `pulse_fit.png`: original, predicted and residual spectra.
- `pulse_resynthesis.wav`: independently synthesized pulse hypotheses.
- `pulse_mask_estimate.wav`: estimated component using the recording's phase.
- `pulse_mask_residual.wav`: reference minus that mix-derived estimate.

The mask residual RMS is 0.704 times the original excerpt's RMS. This is a
baseline diagnostic, not a fidelity score or a percentage of music recovered.
The inspected plot shows sustained low harmonics captured in the first portion,
but much broadband material remains; the changing low-frequency contours around
15–20 seconds are poorly represented by fixed-note candidates. No listening
approval or recovered instrument identity is claimed.

Validation checks an independently generated, phase-shifted pulse for pitch,
duty and gain recovery; also verifies the estimate and residual recombine to
the input and synthesis is finite. Next: add continuous pitch trajectories and
a noise/wave model, enforce temporal voice continuity, and compare the resulting
excerpts before selecting a target sample bank. No game code changed and no new
FS-UAE audio validation is claimed yet.

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

## Listening feedback and arpeggio investigation

The user confirms that beat tracking sounds accurate for all three tracks.
That listening acceptance is recorded separately in `listening_review.json` so
regenerating automatic estimates will not erase it. Retain the current beat
grids; the local onset-phase diagnostic is not a reason to override this review.
Meter, downbeat labels and PC cue alignment are still separate questions.

The user rates the instrument fitting as poor and suspects very rapid arpeggios.
This is a strong hypothesis to investigate, not yet a recovered table sequence.
The first model's 4096-sample window at 22050 Hz spans 185.8 ms; its approximately
10 ms hop does not undo that temporal averaging. Several successive pitches can
therefore be fitted as simultaneous harmonics or an incorrect duty shape. The
five-frame median filter and 60 ms event cutoff can additionally suppress short
steps. Its candidate events should not be used as transcription input.

`tools/audio/inspect_arpeggios.py` creates 185.8, 46.4 and 23.2 ms spectrogram
comparisons with a 2.49 ms hop for each track's 0–4 and 15–19 second intervals.
It also renders a known single-voice arpeggio changing pitch every 20 ms as a
resolution control. Outputs: `scratchpad/audio/<track>/arpeggio_resolution_*.png`
and `scratchpad/audio/arpeggio_control.png`. Short windows improve temporal
localization at the cost of low-frequency pitch resolution; they are not a
standalone solution to polyphonic transcription.

Revised fitting order:

1. Locate rapid transitions using short windows and high harmonics; use longer
   windows only where the signal is locally stable or to constrain low pitches.
2. Fit sequences of short pitch steps and repeating interval patterns jointly
   over longer phrases. Allow abrupt arpeggio jumps, rather than penalizing all
   pitch discontinuities as errors. Infer table rate/phase separately from beats.
3. Compare those sequences against competing sustained, sliding, vibrato and
   waveform-changing explanations. Require repeated evidence and a better
   independently resynthesized excerpt, not merely stronger spectral masking.
4. Keep a parent note, its envelope and its rapid pitch/wave sequence separate
   in the event representation. Test whether each step retriggers or continues
   the envelope/phase. Do not turn every fast step into a fresh instrument note.
5. Only fit instrument timbre after accounting for temporal pitch changes.
   Do not assume the current pulse-duty estimates identify the original sounds.

The existing synthetic pulse check establishes basic dictionary mechanics only;
it does not validate accuracy on rapidly changing or mixed instruments.

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

## Third fitting experiment: short-window voice paths

`tools/audio/fit_sequences.py` fits two pulse hypotheses with a fixed duty per
voice, 46.44 ms windows and 2.49 ms hops. A dynamic-programming path allows pitch
jumps of any interval with a small switching cost. Rendering uses one pitch at
a time per voice with continuous phase and no per-step envelope retrigger.
It removes the prior median filter and 60 ms event cutoff.

```sh
OPENBLAS_NUM_THREADS=1 MPLCONFIGDIR=/private/tmp/hexagon-matplotlib venv/bin/python tools/audio/fit_sequences.py
```

Requires the previous analysis and pulse-fit outputs. New listening files under
`scratchpad/audio/courtesy/` are `sequence_resynthesis.wav`,
`sequence_voice_0.wav`, `sequence_voice_1.wav` and `sequence_mask_residual.wav`.
Events and metrics are in `courtesy.sequence_candidates.json`.

Result: this experiment is **not an improvement established by measurement**.
Independent resyntheses compared with the recording using the same 2048-sample
STFT have gain-aligned magnitude errors of 0.738526 (old) and 0.738728 (new),
normalized by reference magnitude norm. These are diagnostic errors, not
perceptual scores. The new model generates 948 pitch segments in 20 seconds,
showing excessive fragmentation. Both pulse paths can still explain the same
source's harmonics; they are not identified Game Boy channels.

An independently generated pulse arpeggio changing pitch every 40 ms achieves
100% pitch accuracy at evaluated step interiors. Transition frames are excluded
from this test; it establishes basic temporal tracking on a simple isolated
signal, not correctness on mixtures or faster steps. Output samples are finite.

The user-approved beat maps remain unchanged. Rapid arpeggios remain a plausible
explanation, but this result does not confirm them or identify their rate.
Next fitting work should explicitly search repeating interval/table sequences
across repeated phrases, compare against time-varying waveform alternatives,
and handle noise and shared harmonics. Do not export these fragmented candidates
to an Amiga sample bank. No game code or target playback changed.

## Fourth fitting experiment: repeating patterns with validation

`tools/audio/fit_repeating.py` compares one held pulse, a repeating pitch
sequence with fixed duty, and a repeating duty sequence with fixed pitch.
It searches 2/3/4/6/8-step patterns, step lengths 10–80 ms in 5 ms increments,
and four phases per step. The analysis window is 23.22 ms. Rates faster than
this window remain blurred; a fine hop is not proof of adequate resolution.
Duty switching is only a restricted waveform-change alternative, not a full
LSDJ wave-channel model.

Parameters and spectral weights are selected using Courtesy 0.2–1.6 seconds.
A separate 1.7–2.6 second interval evaluates each train-selected candidate;
validation does not choose rates or patterns. Amplitudes are fitted per frame
on both intervals, so this measures generalization of spectral shape/pitch
patterns, not a blind prediction of envelopes or a full audio reconstruction.

| Model | Validation weighted residual fraction (lower is better) |
| --- | ---: |
| Held pulse | 0.6064 |
| Repeating pitch | 0.6237 |
| Repeating duty | 0.6095 |

The pitch cycle wins slightly on training data but loses on validation. Do not
interpret the selected 80 ms cycle as a recovered arpeggio. All three candidates
focus on low pitches; the held candidate is MIDI 44. Low-frequency resolution,
shared harmonics and other sources in the mix remain major limitations. These
results do not rule out rapid arpeggios in other components or passages.

A separate synthetic pulse sequence (three pitches, one step every 40 ms,
independent phase) checks the comparison: the repeating-pitch model beats both
alternatives by more than 10% on validation explained spectral energy. This
control is simpler than the recording and does not establish mixture accuracy.

Run after the initial decode:

```sh
OPENBLAS_NUM_THREADS=1 MPLCONFIGDIR=/private/tmp/hexagon-matplotlib venv/bin/python tools/audio/fit_repeating.py
```

Compact results are in `courtesy.repeating_comparison.json`. Diagnostic previews
under `scratchpad/audio/courtesy/` are `repeating_held.wav`,
`repeating_pitch_cycle.wav` and `repeating_duty_cycle.wav`, all covering the first
three seconds with a common anti-clipping gain. These are single-component
hypotheses, not soundtrack replacements or isolated source channels.

Next: investigate the remaining harmonic components with a broader waveform
model and separate frequency-band evidence before attempting another full
transcription. Preserve the accepted beat grids. No new Amiga playback assets
were produced in this experiment.

## Tail-suppression experiment

The user's impression of reverb prompted a bounded preprocessing experiment on
Courtesy 0–20 seconds. This does not establish that the recording contains room
reverb, estimate RT60, or distinguish delay from instrument envelopes/retriggers.
The original recording and accepted beat grids remain unchanged.

`tools/audio/test_tail_suppression.py` uses a delayed, exponentially smoothed
spectral-energy history, attenuating bins that fall below that history. A shared
stereo gain mask preserves relative channel spectra; smoothing and a gain floor
limit suppression. This is a simple heuristic, not an implementation of a
published blind dereverberation algorithm. Spectral suppression is motivated by
[late-reverberation research](https://pubmed.ncbi.nlm.nih.gov/21428508/), which
also evaluates music, but its reported results do not validate this prototype.
Sustained notes and repeated arpeggios can contaminate the history estimate.

Run after decoding:

```sh
OPENBLAS_NUM_THREADS=1 MPLCONFIGDIR=/private/tmp/hexagon-matplotlib venv/bin/python tools/audio/test_tail_suppression.py
```

Listening outputs are in `scratchpad/audio/courtesy/tail_test/`:

- `original.wav`: unprocessed 20-second excerpt, resampled to 22.05 kHz stereo.
- `decay160_floor70.wav`: conservative starting preview.
- `decay160_floor70_removed.wav`: exactly what that pass removed, at original gain.
- Other previews test 80/300 ms history decays or a lower 0.5 gain floor.

History decay is an experimental setting, not measured acoustic decay. No preview
is independently normalized; all can be compared at the same playback volume.
The removed signal should be auditioned for quiet notes and arpeggio steps, not
assumed to contain only ambience.

| Input | RMS relative to original | Pulse magnitude-fit error |
| --- | ---: | ---: |
| Original | 1.000 | 0.65769 |
| 80 ms history, 0.7 floor | 0.936 | 0.66384 |
| 160 ms history, 0.7 floor | 0.938 | 0.66373 |
| 300 ms history, 0.7 floor | 0.939 | 0.66399 |
| 160 ms history, 0.5 floor | 0.924 | 0.66669 |

Each input is refitted using the same 1024-sample pulse dictionary and normalized
magnitude-error calculation. This is not the independent-resynthesis metric from
the earlier experiments and should not be compared numerically to that metric.
All variants slightly worsen this diagnostic; none earns adoption as the default.
That does not disprove reverb or establish that no other removal method can help.

Local onset peaks have median measured movement zero and 95th percentile about
15 ms. The search is restricted to ±15 ms, so this is not proof that attacks
are preserved: disappearing peaks can match a search boundary. Listening review
and a labelled note test are still needed before using processed input to
transcribe. The test counts spectral onsets, not reviewed musical notes.

Controls pass: a dry sustained tone retains 99.89% RMS in the checked interior;
a known synthetic decaying tail is reduced to 70.52% RMS. Array lengths, finite
outputs and mask floors are checked. Neither control validates mixed music.
Full measurements are in `courtesy.tail_test.json`.

Next decision: listen to the original, conservative preview and removed signal.
If useful attacks are not clearer or musical content is removed, keep the dry
estimation branch optional and return to joint instrument/ambience modeling.
No game code, beat maps or Amiga playback assets changed.

## Beat-aligned sample reuse: first full-track experiment

The alternative sample-reuse approach now has a reproducible full-track pilot
for Courtesy. Run after decoding:

```sh
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/reuse_beats.py
```

The approved candidate grid is retained as the slice origin. Three slice sizes
(half, one and two beats) are evaluated at total storage budgets of 32, 64, 128
and 192 KiB. Every preview spans the full 193.333375-second resampled recording,
including the incomplete introduction and ending, which are retained verbatim.

Audio is mono 16 kHz quantized to signed 8-bit with one common gain. Compare
against `scratchpad/audio/courtesy/beat_reuse/reference_16k_8bit.wav`, not only
the original MP3: that separates downsampling/mono/quantization loss from slice
substitution. Preview WAVs use a 16-bit container but contain only 8-bit levels.
There is no independent slice loudness normalization, crossfade or added silence.

The dictionary contains actual recording slices chosen by a greedy reduction of
aggregate time-varying spectral error. Descriptors retain loudness and frequency
content. This is approximate, lossy reuse, not verified musical equivalence or
a globally optimal dictionary. It does not yet search fine time offsets,
transpositions, different beat-grid phases or cross-track reuse. Rounded slice
lengths can differ by one sample; host preview reconstruction interpolates that
one-sample difference to preserve the absolute timeline. Target playback must
implement an equivalent timing policy; these files are not a finished player.

### Results at 192 KiB

| Slice duration | Source slices | Exact unique slices | Dictionary entries | Accounted bytes | Feature MSE |
| --- | ---: | ---: | ---: | ---: | ---: |
| Half beat | 837 | 824 | 51 | 195,396 | 0.0257 |
| One beat | 418 | 413 | 25 | 193,668 | 0.0291 |
| Two beats | 209 | 208 | 12 | 185,398 | 0.0324 |

Exact uniqueness is computed on the quantized slice bytes, with no alignment
search. It does not imply that all these passages sound different. Nearly all
slices are byte-distinct, so exact deduplication alone is insufficient here.
Half-beat dictionaries have the lowest feature error among these tested choices;
this is not listening approval and does not establish preservation of melody,
drums or arrangement. Waveform errors are large and phase-sensitive; they are
reported for transparency, not treated as perceptual scores.

At 192 KiB, the 95th-percentile immediate join jumps are 24, 25 and 15 signed
8-bit amplitude units respectively, versus 12 in the corresponding reference.
These increased discontinuities flag possible clicks. No joins have been hidden
with crossfades; doing so would require explicit buffer/channel/timing costs.

### Outputs and memory accounting

`soundtrack/courtesy.beat_reuse.json` records all 12 trials. Under
`scratchpad/audio/courtesy/beat_reuse/`, each configuration has:

- `<beats>beat_<budget>k.wav`: full-track reconstruction.
- `<beats>beat_<budget>k.s8`: actual signed-byte sample bank.
- Matching JSON: source slice indices, dictionary assignments, active lengths,
  absolute slice boundaries and bank layout, sufficient to reproduce the preview.

For example, audition `0.5beat_192k.wav`, then `1beat_192k.wav` and
`0.5beat_64k.wav` against the common reference. Those comparisons expose the
quality tradeoff instead of assuming the smallest dictionary is acceptable.

Accounted storage includes word-padded intro/outro samples, word-aligned fixed
sample slots with guard words, and a proposed sequence allowance of four bytes
per event plus a 32-byte header. The sequence is currently emitted as host JSON,
not that packed target representation. Runtime code, seek tables, crossfade
buffers, track switching, OS and loader peaks are excluded. The budget covers
Courtesy alone: fitting all three tracks in the old music allocation has not
been demonstrated. Raw reference audio alone occupies about 3.09 MB at this rate.

Validation: all reconstructions retain the reference sample count and original
edge fragments; each bank plus proposed sequencing allowance fits its budget.
An identity dictionary reconstructs the quantized track byte-for-byte, and a
synthetic repeated-descriptor control verifies exact coverage by two entries.
No Amiga runtime changed; FS-UAE playback remains a later gate after choosing
an acceptable reconstruction and implementing its replay format.

Next: listening review of the full-track previews, then alignment and join
improvements, phrase-aware substitutions and comparable reports for Focus/Otis.
If 192 KiB for one song is audibly inadequate, report that before trying to fit
three songs into the same budget or committing to a player.

## Two simultaneous slices: additive dictionary experiment

`tools/audio/reuse_layers.py` tests Courtesy at 64 and 192 KiB with half-beat
slices. It fits a base dictionary and a correction dictionary using alternating
waveform assignments and centroid updates (ten iterations, deterministic
initialization). Both dictionaries are quantized to signed 8-bit throughout
fitting, and clipping to that representation is part of the experiment.
A comparable single-dictionary waveform baseline uses the same fitting method.
These are synthesized dictionary averages, not isolated musical source layers.

The two-layer sequence allowance is six bytes per slice (two sample IDs and
length), versus four for one layer; both include the same header, padded sample
slots, guards and verbatim incomplete intro/outro. Runtime and overlap buffers
remain excluded. Assignment alternates between dictionaries and can converge to
a local solution; it does not exhaustively optimize all base/correction pairs.
Samples are aligned only to the approved grid with one-sample length correction.
No fine time-shift/carrier-phase optimization is implemented in this experiment.

| Budget | Base + correction entries | Accounted bytes | Waveform relative error | Feature MSE |
| --- | ---: | ---: | ---: | ---: |
| 64 KiB | 15 + 0 | 62,412 | 0.7756 | 0.1141 |
| 64 KiB | 7 + 8 | 64,090 | 0.8864 | 0.1299 |
| 64 KiB | 11 + 4 | 64,090 | 0.8932 | 0.1329 |
| 192 KiB | 51 + 0 | 195,396 | 0.7006 | 0.0967 |
| 192 KiB | 25 + 25 | 193,380 | 0.7293 | 0.1017 |
| 192 KiB | 37 + 13 | 193,380 | 0.7630 | 0.1061 |

Lower errors are better under each metric. Neither split beats its one-layer
waveform baseline. The previous actual-slice spectral dictionary has much better
feature MSE (0.0408 at 64 KiB; 0.0257 at 192 KiB), although worse waveform error.
This illustrates how averaging differently phased recordings can improve sample
error while damaging spectral content. No perceptual improvement or compression
advantage is established. It does not rule out a better aligned or musically
separated two-layer representation.

Run:

```sh
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/reuse_layers.py
```

Outputs under `scratchpad/audio/courtesy/layer_reuse/` include the reference,
previous single-layer previews, and each trial's mix/base/correction WAVs, signed
sample bank and assignment/layout JSON. All listening previews in this directory
use a common half-gain, including the reference and previous reconstructions,
so two full-range layers can sum without clipping. Do not compare their loudness
against files in the earlier directory without accounting for this gain.
The combined preview is the exact arithmetic sum of its exported layers.

Start with `reference.wav`, `previous_192k.wav`,
`192k_51base_0correction_mix.wav` and `192k_25base_25correction_mix.wav`.
The correction-only file is a signed difference signal, not percussion or melody.
No Amiga stereo routing, hardware mixer, replay timing or source separation is
claimed. Current output is mono; an Amiga implementation would need to choose
routing and headroom explicitly.

Next: retain the previous spectral dictionary as the best measured spectral
baseline. Test fine alignment and better joint selection before expanding this
additive approach to other tracks. Continue to judge actual musical content by
listening; waveform error alone is an unsuitable selection criterion here.

Validation: all six stored banks and assignment files were decoded independently
and reproduce every interior preview sample exactly. Each mix equals the sum of
its exported layers, each output has 3,093,334 samples at 16 kHz, and bank plus
sequence allowances fit the stated budgets. No Amiga runtime code changed.

## Codec experiment: measured host quality/storage, target timing pending

`tools/audio/codec_experiment.py` compares the first 20 seconds of all three
tracks at mono 16 kHz. Each track has one common anti-clipping gain and an 8-bit
quantized reference. Preview WAVs use a lossless 16-bit container for those
8-bit output values. Methods:

- Fibonacci delta: four-bit codes, fixed delta table, signed-byte wrap semantics,
  predictor restart every 512 samples. Custom `FIB1` container, not an 8SVX file.
- IMA ADPCM: FFmpeg's WAV encoder/decoder; decoded samples are rounded/clipped
  to signed 8-bit to represent final Paula output. The exact FFmpeg version is
  recorded. Padded encoded blocks count toward storage; previews trim the tail
  to the original reference length. No native IMA decoder has been added yet.
- VQ8: 256 quantized eight-sample vectors, one byte per vector index. A 2 KiB
  codebook is trained on up to 5,000 blocks from the first ten seconds only;
  second-half metrics test unseen material. This is a basic waveform codebook,
  not a perceptual codec or a full-track-trained dictionary.

| Codec | Encoded bytes / 20-second excerpt, including container | Reduction |
| --- | ---: | ---: |
| Reference 8-bit PCM payload | 320,000 | 1× |
| Fibonacci | 160,637 | 1.99× |
| IMA ADPCM WAV | 160,862 | 1.99× |
| VQ8 | 42,056 | 7.61× |

| Track | Fibonacci SNR | IMA SNR | VQ8 SNR |
| --- | ---: | ---: | ---: |
| Courtesy | 22.78 dB | 18.86 dB | 9.64 dB |
| Focus | 22.19 dB | 24.40 dB | 13.70 dB |
| Otis | 19.36 dB | 13.24 dB | 12.06 dB |

SNR is relative to the quantized/downsampled reference, not the original MP3.
These comparisons do not establish subjective quality or represent each whole
song. Fibonacci wins the measured error on Courtesy/Otis; IMA wins on Focus.
The short-block dictionary saves much more storage but has substantially more
error. Decoder CPU costs have not been measured on the A500.

Reproduce:

```sh
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/codec_experiment.py
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/compress_beat_bank.py
```

Listening files are under `scratchpad/audio/<track>/codec_test/`: reference.wav,
fibonacci_preview.wav, ima_adpcm_preview.wav and vq8_preview.wav. Actual coded
files accompany them. Measurements are in `soundtrack/codec_comparison.json`.

### Larger reusable bank within the same budget

The follow-on experiment applies Fibonacci coding to independently stored
Courtesy half-beat samples, keeping the previous spectral-medoid selection.
Predictor state resets at each coded block; sample offsets allow seeking.

| Total budget | Old PCM entries | Compressed entries | Accounted bytes | Old feature MSE | New feature MSE |
| --- | ---: | ---: | ---: | ---: | ---: |
| 64 KiB | 15 | 30 | 64,624 | 0.0408 | 0.03193 |
| 192 KiB | 51 | 100 | 195,489 | 0.0257 | 0.01972 |

New accounting includes actual compressed samples, verbatim intro/outro,
four-byte sample offsets, proposed compact sequencing/header storage and
1,536 bytes for three 512-byte PCM buffers. Decoder code/state, interrupt setup,
allocator/OS costs and loader peaks remain excluded. The earlier PCM comparison
did not reserve those decoded buffers. This is Courtesy only, not three songs.

Under `scratchpad/audio/courtesy/compressed_beats/`, audition
`192k_fibonacci.wav` against `192k_before_codec.wav` (identical larger dictionary
before delta coding), and the earlier `beat_reuse/0.5beat_192k.wav`. All use the
same reference gain. Compressed banks, edge-fragment PCM and complete host
sequence/offset JSON are retained. Compact target sequencing is budgeted but
not yet implemented. One-sample slice-length correction remains a host-preview
operation which the target must reproduce with an explicit timing policy.

Validation: independent known-byte vectors check Fibonacci wrap behavior and
VQ lookup; boundary-length cases exercise Fibonacci blocks. Both stored full-track
compressed banks and their sequences replay byte-for-byte to their exported
previews. The new spectral error is lower, but joins and musical substitutions
still require listening review.

Next target gate: implement a bounded 68000 Fibonacci decoder and replay
scheduler, verify it against these host bytes, then measure worst-case block
fill time and underruns under heavy game rendering in PAL/NTSC FS-UAE. No claim
of real-time feasibility, new ADF or emulator audio verification is made in this
host-only batch. Do not replace LSP until that gate passes.

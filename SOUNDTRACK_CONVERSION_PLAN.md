# Soundtrack conversion plan

## Objective and baseline

Reconstruct each supplied recording as compact musical events and reusable
instruments suitable for stock A500 playback. Preserve arrangement, rhythm,
pitch, characteristic timbres, loops and PC game synchronization. Analysis and
synthesis preparation run offline; the game plays precomputed data.

Inventory measured with ffprobe, 2026-09-19:

| Input in assets/ | Container duration | Audio |
| --- | ---: | --- |
| Chipzel - Courtesy [3353237090].mp3 | 193.333 s | 44.1 kHz stereo MP3 |
| Chipzel - Focus [3161739429].mp3 | 162.222 s | 44.1 kHz stereo MP3 |
| Chipzel - Otis [1413220136].mp3 | 156.444 s | 44.1 kHz stereo MP3 |

These durations do not establish musical boundaries or game loop lengths.
Current audio.c embeds technova_main.lsmusic/lsbank and uses the CIA-driven
LightSpeedPlayer. LSPConvert is present locally. The existing music occupies
about 193.4 KiB; the whole game payload plus explicit allocations is about
290.9 KiB, excluding OS, stack and allocator overhead. This is a replacement
budget, not space to add three similarly sized banks. No original soundtrack
LSDJ project was found in the initial repository search.

## Reference model and limits

The official [LSDJ manual v4.8.6](https://www.littlesounddj.com/lsd/latest/documentation/LSDj_4_8_6.pdf),
sections 1.3, 2.5–2.10 and 5, describes two pulse channels, a wave channel and a
noise channel. WAVE supports evolving synth frames; KIT and SPEECH also use the
wave channel. Kits can mix two samples. Tables change pitch, amplitude and
commands; grooves control step durations, potentially independently by channel.
Thus four hardware channels do not imply four fixed instruments or four cleanly
separable musical parts. The recording's actual LSDJ version remains unknown;
version-specific behavior must be checked before implementing an exact model.
Consult the author's [documentation sources](https://github.com/jkotlinski/lsdj-doc)
for clarification, while retaining a versioned reference.

Treat channel assignments and settings inferred from MP3 as hypotheses. Stereo
mixing, shared harmonics, compression and any mastering can make multiple
reconstructions equally plausible. A plausible equivalent performance is the
fallback when exact original settings cannot be recovered. Never label an
estimated stem or inferred note as ground truth. Original .sav/.lsdsng projects,
if available later, would provide a much stronger route: export events and
isolated channels, then align them to the supplied recordings.

## 1. Establish reproducible references

- Hash and inventory inputs; preserve originals. Decode once to lossless PCM,
  retaining stereo and decoder/encoder delay metadata. Record all offsets and
  tool versions. Do not silently trim intros, silence or tails.
- Produce waveforms, multiresolution spectrograms, stereo correlation and
  onset-strength plots. Listen through all three; mark sections, exposed
  instruments, dense passages, transitions and possible overdubs/effects.
- Check whether these are the same edits used by the PC game. Use existing PC
  evidence to establish track-to-level mapping, start offsets, loop boundaries,
  speed changes and cue timing. Album timing must not become game timing by
  assumption.
- Deliver a manifest and annotated reference timeline per track. Keep large
  derived PCM/spectrogram files outside tracked source; retain regeneration
  scripts and compact annotations.

Exit: every comparison has a documented time origin, and representative excerpts
are chosen from actual inspection rather than guessing which track is easiest.

## 2. Recover rhythm and arrangement

- Combine transient detection, periodicity and pitched-note changes to propose
  beats. Test half/double-tempo alternatives and manually confirm downbeats.
- Fit a tempo map, bar structure and step grid using multiple sections. Preserve
  swing, microtiming and tempo changes; do not quantize everything to one BPM.
- Look for repeated phrases and transpositions. Use these repetitions to improve
  weak onset estimates and expose instruments masked in other occurrences.
- Export beat/downbeat times, phrase boundaries and confidence flags. Audition
  click overlays at the beginning, middle and end to catch accumulated drift.

Exit: a reviewed full-song timing map for each file, with uncertain passages
explicitly marked. Game pulse cues remain separate from inferred musical beats.

## 3. Build and validate a constrained instrument model

- Implement an offline reference renderer with pulse duty variants, amplitude
  envelopes, pitch trajectories, wave-frame sequences and noise modes. Validate
  hardware-specific details against documentation and known isolated examples.
- Include sampled percussion and composite wave-channel events when evidence
  demands them. Do not force every drum into a noise model.
- Learn candidate instruments from exposed notes and repeated passages. Estimate
  tuning, envelope, spectral shape, modulation and pan. Separate plausible
  instrument behavior from recording EQ, filtering or distortion.
- Prefer generating clean reusable samples from fitted parameters. Extract
  samples from the mix only where isolation is credible; label residual bleed.
- Maintain both a high-quality reference synthesis and an Amiga-constrained
  rendering so transcription errors and target-hardware compromises can be
  assessed independently.

Exit: a small auditionable instrument dictionary with evidence timestamps,
parameter ranges, confidence and examples it explains.

## 4. Jointly reconstruct channels and note events

- Use stereo position and harmonic continuity as clues, not guaranteed stem
  separation. Fit a small pulse/wave/noise dictionary to overlapping spectra,
  then optimize temporal continuity and channel occupancy.
- Iterate event estimation and resynthesis. Compare the predicted combined mix
  with the recording; inspect the residual for missed notes, percussion and
  modulation. Use multiple time resolutions for bass and fast arpeggios.
- Enforce plausible channel behavior while allowing rapid instrument changes,
  kit mixing and possible recorded overdubs. Two pulse-channel identities may
  remain interchangeable; musical correctness matters more than arbitrary labels.
- Distinguish retriggers, legato pitch changes, slides, vibrato, arpeggios and
  waveform changes. A single MIDI note plus a static sample is often inadequate.
- Save an editable intermediate representation: time, duration, logical channel,
  instrument, pitch/volume curves, waveform changes, pan, loop state, source
  interval and confidence. Keep manual corrections separate from generated fits.
- Render solo estimated channels and a combined mix for review. Generic neural
  source separation may assist difficult passages, but its output is not truth
  and is not the primary route for these sounds.

Exit: reviewed events, instruments, stems and a reference resynthesis. Prototype
this on 15–30 seconds first, including an exposed and a dense passage, before
transcribing entire songs.

## 5. Prove an Amiga playback route with the prototype

Use [Commodore's Audio Hardware reference](https://www.amigarealm.com/computing/knowledge/hardref/ch5.htm)
for Paula's four DMA channels, sample/period constraints and fixed stereo
routing. Matching channel counts alone does not guarantee matching sound.

- First test the existing LSP pipeline with short generated instruments and
  event sequences. Audit what the local converter actually preserves: pitch
  range, sample changes, effects, timing and loops. Do not commit to MOD/LSP as
  the transcription format before this test.
- Generate signed 8-bit samples with valid word alignment, lengths and loops.
  Use reusable cycles for stable tones; waveform banks or short baked segments
  for evolving sounds; bounded one-shots for percussion. Choose octave-specific
  samples where Paula period limits or aliasing require them.
- Quantify tuning, envelope and timing error. Check both PAL and NTSC clocks,
  loop clicks, volume quantization, filtering and stereo differences.
- If LSP cannot preserve essential modulation, compare a small CIA-timed event
  player against baking more of each instrument into samples. Measure CPU and
  Chip RAM before selecting. Avoid full real-time Game Boy emulation or a
  software mixer unless the prototype demonstrates a need and a viable budget.
- Explicitly choose how music shares Paula with announcer/effect samples:
  temporary channel stealing, precombined events or measured software mixing.
  Four-channel music does not leave a free fifth channel.

Exit: one representative excerpt playing in FS-UAE alongside worst-case game
rendering, with an audio capture, CPU cost and memory report. Resolve this gate
before scaling transcription to all three songs.

## 6. Complete tracks and integrate game behavior

- Finish one full track, validate its loop and long-term timing, then repeat the
  workflow for the other two. Reuse instruments and phrases across songs where
  the evidence supports it.
- Report sample-bank, event-stream, player and scratch-memory costs separately.
  Initially aim for all soundtrack assets together to fit within the existing
  approximately 193 KiB music allocation; this is a design target, not a promise.
- Prefer shared banks and compact event streams. Evaluate all-resident tracks
  first. If track loading is needed, design a safe OS/cooperative loading phase;
  do not introduce floppy I/O during the existing system takeover by accident.
- Add track selection, start/stop, loop and position APIs. Implement PC start
  offsets with seek checkpoints restoring instrument, envelope and modulation
  state, rather than jumping to a note index with missing sustained notes.
- Drive gameplay synchronization from the verified playback timeline, with
  measured latency compensation. Confirm retry, death/fade, menu return,
  continuation and random-start behavior against the PC evidence.
- Preserve audio shutdown and system restoration. Measure peak track-switch and
  packed-loader memory as well as steady-state use.

## 7. Acceptance and regression checks

For every song retain original, reference resynthesis and FS-UAE capture for
aligned listening comparisons. Check mono and stereo, full arrangement and
isolated estimated channels. Use spectrograms, onset errors, pitch trajectories,
loudness and residuals to find mistakes; waveform subtraction alone is not a
reliable quality score for lossy recordings or phase-sensitive synthesis.

Provisional engineering targets: no missing sections or conspicuous wrong notes;
review all low-confidence events; onset agreement within one chosen event tick
where attacks are identifiable; no accumulating timing drift exceeding one tick
over a full song. Record exceptions and listening judgments, rather than claiming
sample-identical reproduction. Final audible fidelity needs human review.

Run stock 68000 FS-UAE PAL 512 KB Chip-only and NTSC configurations, with the
game's heaviest rendering and rapid start/stop/seek/track changes. Capture audio
and check pitch, duration, glitches, interrupt load, memory headroom and clean
exit. Follow with real A500 listening/timing checks when available.

## Proposed working layout and first deliverable

- tools/audio/: inventory, analysis, model fitting, renderer and target exporter.
- soundtrack/: compact manifests, timing maps, instruments, events and manual edits.
- scratchpad/audio/: decoded PCM, plots, stems, comparisons and emulator captures.
- tests/audio/: small synthetic timing/effect fixtures and conversion checks.

First deliverable: inventory plus reviewed timing maps and a representative
15–30-second excerpt reconstructed as four logical channel streams, with a small
sample bank, reference audio and an FS-UAE playback comparison. Its measured
fidelity and resource costs determine the backend and full-track schedule.

Status: first automatic analysis batch complete; see soundtrack/README.md.
Beat-grid hypotheses and listening previews exist for all three recordings.
Timing review, channel reconstruction and target playback remain pending.

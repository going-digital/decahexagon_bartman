# Music-driven radial pulse

The old 132-BPM whole-scene zoom has been removed. The PC uses a stored 60 Hz
cue envelope, not live beat detection. `assets/music1.cues` holds the original
11,441 byte-sized Courtesy cues (range 0–80); `pc_cues.json` records provenance.
`tools/audio/extract_pc_cues.py` reproduces it from the SHA-256-checked executable.
The extra table lives in ordinary CPU memory, not a mandatory Chip allocation.

## Verified PC behaviour

`superhex::gamelogic`, `0x100058196`, indexes musicclass+0x18 at
`floor(song_position_ms * 60 / 1000)`. Playback offsets affect that position.
At `0x100058290` it takes `floor(abs(cue)/2)`; stage 2 uses `/3` instead.
For the port's dt=1 simulation: a rising target replaces the envelope then loses
one unit; otherwise the envelope loses two units, clamped to zero. The stage-2
branch has the same decay. Scheduler rotation cues independently set it to 12.

The renderer reads gameclass+0x29b0 (for example `0x10004924c`), adding it to
radial coordinates. Hub, player and both wall edges now receive that same
addition before camera scaling. The player's centre translates without widening its triangle (PC scene builder
`0x100049a77`). Wall thickness and collision geometry are unchanged. Palette changes remain on their existing PC schedule; they are not
artificially retriggered by each music cue.

The source MP3 is 51 ms later than PC music1.dat. Waveform cross-correlation at
10, 30, 90 and 170 seconds gives the same offset, with correlations over 0.9.
At 12 kHz, lookup therefore subtracts 612 samples before dividing by 200.
Negative positions select the initial zero cue; positions beyond the extracted
table return zero. The accepted reconstruction's longer ending remains intact.
Reproduce the independent alignment check with:

```sh
venv/bin/python tools/audio/verify_cue_alignment.py /path/to/data/music/music1.dat
```

## Playback clock

Each prepared 512-byte PCM buffer carries its original song sample position.
The audio interrupt publishes the position of the buffer that has just begun
playing, not the producer's position several buffers ahead. A raster/VBlank
stamp interpolates within that buffer using Paula's PAL/NTSC period, clamped at
its last sample if an interrupt is delayed. The clock reanchors on every audio
interrupt, so the hardware rate's small difference from 12,000 Hz does not cause
accumulating drift. Scanline granularity and integer arithmetic give roughly
sample-level interpolation; rendered display timing remains frame-quantized.

Retry restarts both PCM and cue position. Sequence looping wraps at the actual
PCM length, including a loop within a DMA buffer. If a buffer repeats on an
underrun, the cue follows that repeated audio rather than advancing silently.
Fade-out keeps cues running while PCM plays; after stop the pulse decays to zero.
Silent builds no longer pretend to have a periodic musical beat.

## Scope and checks

This follows the accepted Courtesy soundtrack on all selectable profiles, as
the current player does. It does not add the other songs or the PC's randomized
retry offsets. The PC's stage-2 scripted camera freeze overrides the cue to 60;
that override belongs to the still-unimplemented camera/ending state and is not
claimed here. Full PC perspective/tilt and ending scenes remain separate work.
Lossy slice substitution can change individual attacks; original cue timing
cannot make the reconstruction identical to the original recording.

`make test-pulse` checks all cue values -120..120, all three stages and prior
envelopes 0..80 against an independent transcription of the PC float arithmetic,
plus sample offset boundaries, PAL/NTSC interpolation, delayed IRQ clamping,
retry and looping inside a DMA block. Existing menu, lifecycle and SFX checks
also pass. Target observations are recorded below.

### FS-UAE A500 observations

Both trials used 512 KiB Chip + 512 KiB Slow memory with effects enabled:

- PAL: 45.51 seconds, 1,065 music blocks, **0 underruns**. Retry returned to the
  start of the soundtrack; menu return and exit to AmigaDOS succeeded. This
  trial preceded the final correction that translates the player without
  widening its triangle.
- NTSC, final player geometry: 45.56 seconds, 1,071 blocks, **0 underruns**.
  Startup/death were also exercised in an earlier NTSC run.

Binary hashes and screenshots are in `pulse_target_trials.json`. These are
rendering/lifecycle/underrun observations, not a captured audio/video phase
comparison with a running PC game. A full-song loop has not yet been observed
in these target runs; position arithmetic at the loop is covered by host tests.

Final cheat-free PAL build: `out/pcm_pulse.adf` (476,844-byte executable).
The ordinary effects-only build also compiles and passes the release cheat
audit; no cue table is linked when PCM music is disabled.

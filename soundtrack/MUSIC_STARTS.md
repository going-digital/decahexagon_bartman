# Soundtrack starting positions

The PC version starts the first gameplay track at zero, then chooses one of
several fixed entry points on subsequent starts. For track 1, Courtesy, those
positions are **0, 27,477, 80,410 and 110,000 milliseconds**. Zero remains a valid
random choice, and repeated choices are allowed. The play counter lasts for the
application session; returning to the menu does not reset it.

This was verified from the installed PC executable, SHA-256
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`:

- `musicclass::play(int)` at `0x10005d6b0` checks the counter at object offset
  `0x1da64`. At zero it plays from zero; otherwise track 1 uses `ofRandom(4)`.
- Table `0x1001bbb30` contains the other three offsets: 27477, 80410, 110000.
  The resulting offset is passed to `ofSoundPlayer::setPositionMS` and retained
  at object offset `0x1da84`; the counter increments after gameplay-track starts.
- Constructor instructions at `0x10005c32c` initialise the current track to -1
  and the adjacent counter to zero. Requests for the already-current track
  return without restarting; the port's existing lifecycle decides when to
  restart its DMA stream.

Local inspection evidence is retained in
`scratchpad/pc_verification/evidence/fresh/musicclass_play_10005d6b0.c` and
`binary_disassembly.txt` in the parent evidence directory. The raw table values
were independently read from the installed Mach-O's mapped segment.

The Amiga currently embeds Courtesy for all profiles. This change reproduces
Courtesy's entry-point selection; it does not add the other PC tracks or their
separate offset tables. A private 16-bit music PRNG is seeded from the frame
counter on the first successful start, so audio choices do not consume or alter
wall-pattern RNG. Exact random sequences are not expected to match the PC.

`fib_song_seek` locates the requested logical sample directly in the PCM slice
sequence, including split memory banks and odd sample positions. It never plays
slice padding or decodes/discards the preceding minute of music. Milliseconds
map to the embedded 12 kHz timeline, giving samples 0, 329724, 964920, 1320000.
PAL and NTSC retain their existing hardware sample-rate approximation. All four
DMA buffers are primed from the new position; the audible position and buffer
position tracking start there too, keeping pulse cues aligned with the tune.
The existing fade-in, fades on death, and stop/menu lifecycle remain intact.

Validation:

- `make test` includes lifecycle/offset selection and seek tests. Every 16-bit
  random input selects a permitted cue, and the first start always selects zero.
- Synthetic seek tests cover FBP1/FBP2, split and contiguous banks, odd positions,
  repeated slices, physical padding, end-to-start wraps and invalid positions.
- An independent flattening of the accepted PCM bank matched 2,520 seek positions
  over 2,048-byte reads, including all four cues and slice boundaries.
- A Copperline target harness restarts every three gameplay seconds, including
  the first zero start, and covers all four offsets in both PAL and NTSC. First
  DMA buffer checksums and queued-buffer positions match the independent PCM
  reference at every start. Gameplay/pulse processing runs alongside the audio.
- The full host suite passed. The release ADF and packed executable were rebuilt
  with the accepted music bank and passed the no-cheat audit.

[Recorded offsets, checksums and provenance](../tests/music_start_results.json).
Audio captures and the independent asset checker are in `scratchpad/music_seek/`;
the target harness is in `scratchpad/frame_benchmark/musicseek/`. Automatic
restarts, survival mode and checksum logging are confined to that harness.

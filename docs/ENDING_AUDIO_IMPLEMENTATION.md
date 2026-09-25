# Ending audio: eightfold stretching investigation

## Secret-ending audio: reversed Focus

The secret ending and F8 preview now load Focus through the existing track cache
and play the entire logical PCM stream backwards at the normal sample period.
Per the user's instruction, the PC's three-semitone pitch difference is omitted.
This is separate from the two-channel stretched Focus used in the Hexagonest
bonus. Reverse playback uses one Paula channel and the existing DMA buffers.

`fib_song_read_reverse` seeks a logical block, copies it across any dictionary
boundaries, then reverses those bytes in the output buffer. Cached samples are
never modified. Production occurs in the main loop; there is no software mixer
or resampler. The end is padded with silence rather than wrapping, and the
controller's stop-music event, Escape and result transition stop playback.

The host audio check verifies the entire reconstructed Focus stream against
its byte-reversed equivalent, including partial final blocks, continued silence
and unchanged cached data. Playback lifecycle tests cover ending/menu handling.
Hardware listening verification remains pending.

## Current playback: Paula hardware overlap

The selected 512-sample aligned schedule now feeds two hardware voices, channels
0 and 3 (the same stereo output). Channel 3 is reserved from effects during
stretch playback; effects retain channels 1 and 2. Stopping restores channel 3
to the effects allocator. Ordinary soundtrack playback still uses channel 0.

Each existing 512-byte Chip slot contains two separate 256-sample segments:
the preceding grain tail and the next grain head. They are copied unscaled.
The first hop plays only the first grain head. No runtime call to the software
crossfade remains; `fib_stretch_read` is retained as a host reference.

Both DMA channels start together. Channel 0 interrupts at each 16-sample block
and queues both channels' next block, with complementary hardware volumes
64/0, 60/4, ... 4/60. This is roughly 750 interrupts/second at 12 kHz.
The envelope is necessarily different from the sample-by-sample Q8 audition.
Grain seeking/copying runs in the main loop, filling all available ring slots.
An underrun repeats immutable data; it cannot expose a partially filled slot.

`tests/paula_grain_queue_test.py` checks the actual queue/envelope code against
fake registers, including initial fetch, volume sums, buffer lifetime, starvation
and recovery. The host PCM test checks every raw segment of all 3,645 hops against
the reconstructed Focus samples, including tail padding and silence. These do
not model Paula's bus timing, channel-start skew or interrupt latency. A real
Amiga/emulator audition is still required before calling hardware playback proven.

Hardware reference: [Commodore audio allocation documentation](https://amigadev.elowar.com/read/ADCD_2.1/Devices_Manual_guide/node0028.html).
The earlier software-mixing implementation notes below describe superseded work.

## Corrected identification and live bonus integration

Track 4 accompanies the playable bonus section at the end of Hexagonest, not
a standalone normal-ending scene. References below to normal-ending audio are
the historical working label. The identified Focus passage remains valid.

The live stage-2 late-phase event now loads/binds Focus and invokes stretched
playback automatically at effective score >7200. This also covers stage handoffs
from other selected profiles. Selected-profile record ownership is unchanged.
Death/menu stop bonus playback; ordinary retries return to their normal track.
`tests/bonus_audio_lifecycle_test.py` checks this distinction from the F8 preview.

Stretch buffer production now runs in the main loop, not VBlank. The target
crossfade explicitly uses signed MULS.W instead of the observed __mulsi3 helper.
Host output still matches all 932,885 samples. This addresses two concrete
performance problems but does not establish the reported freeze's sole cause.
Target timing and hardware playback remain to be measured.

F8 remains the earlier mixed audio/secret-visual audition. It does not exercise
the actual Hexagonest bonus entry, and it is not proof of level completion.

Working hypothesis (2026-09-25): the ending uses Focus stretched to eight times
its duration while retaining pitch. This has not been established by comparison
with the PC ending files. Earlier Courtesy waveform comparisons do not test it.
An eightfold duration is 12.5% tempo; playing samples at 12.5% rate instead lowers
pitch by three octaves. Those are different effects.

### Focus passage located (2026-09-25)

`tools/audio/locate_ending_focus.py` searches the owned PC Focus recording using
96 log-spaced spectral bands, with recording EQ and frame loudness removed.
It tests forward/reverse source order and duration factors 6–10 in 0.05 steps.
The full normal ending (`music4.dat`, 77.7404 seconds) matches **Focus starting
at approximately 24.35 seconds, stretched eightfold**: about 24.35–34.07 seconds
of source. Mean spectral cosine similarity is 0.683; this is not a probability.
Independent target thirds infer source origins 24.350, 24.363 and 24.319 seconds.
These agree strongly, but the spectral windows do not establish sample-accurate
alignment or the original stretch algorithm.

The secret ending (`music5.dat`) does not match convincingly: its best score is
0.156 and independent thirds disagree widely on the source position. Do not use
the normal-ending result to replace secret-ending audio.

Results and listening samples are in `scratchpad/audio/ending_focus/index.html`
and `report.json`. Samples include the PC reference, the original Focus excerpt,
and eightfold FFmpeg atempo previews from both the PC source and the reconstructed
Amiga bank. These previews are not the proposed runtime overlap-add algorithm
and may sound different from the original PC processing. Runtime is unchanged.

### Integer overlap-add listening prototypes

Run `venv/bin/python tools/audio/audition_ending_overlap.py` after the Focus
alignment tool. The audition page is
`scratchpad/audio/ending_focus_overlap/index.html`. It includes the PC reference,
the earlier atempo preview, and ten 8-bit candidates from the reconstructed
Amiga bank: 256/512/1,024/2,048/4,096-sample grains, each with fixed positions or offline
waveform alignment within ±256 samples of the nominal source position.

Each candidate produces exactly 932,885 samples (77.7404 seconds at 12 kHz).
The half-grain output hop and eightfold stretch stay fixed; alignment offsets
do not accumulate into tempo drift. Runtime synthesis is complementary Q8
linear crossfades, with signed rounding specified in the generated report.
The exported big-endian logical-offset schedules occupy 29,156, 14,580, 7,292, 3,648 or 1,824
bytes respectively. Reloading each exported schedule reproduces identical PCM;
source bounds, output length and signed 8-bit limits are checked by the tool.

This is an audition prototype, not target code or a quality verdict. It does
not establish the PC processing algorithm. Opening/final fades and loop joins
remain to be designed; 68000 synthesis cost and playback underruns still need
measurement if a candidate is accepted.

## Existing player

### Amiga audition shortcut

F8 from the title menu now loads/binds Focus through the normal track cache,
starts the current ending visual prototype, and calls the stretch playback API.
Escape returns to selection; ordinary selection/retry loads the selected track
again. The source score/completion state is not advanced by the audition.
This intentionally previews normal-ending audio over the existing secret-ending
visuals, not the unfinished normal-ending scene. Audio ends after 77.7404 seconds;
the 168-second visual prototype continues unless escaped.

Enabling the previously unreferenced schedule exceeded the 192 KiB executable
arena. The native and WHDLoad layouts now reserve 208 KiB, shifting cues/display
heap by 16 KiB and increasing the resident Chip allocation to 376,832 bytes.
Size assertions remain enabled. The distribution build passes relocation and
in-place inflation checks and produces `out/Hexagon.zip` with ADF/WHDLoad editions.
Host PCM equivalence, music lifecycle and ending timeline checks pass. Actual
F8 input, DMA underruns and the enlarged memory layout still need emulator or
hardware playtesting; this is an audition build, not verified release parity.

### Selected implementation: 512 samples, offline alignment

`fib_pcm.c` now implements `fib_stretch_init/read`: 512-sample grains,
256-sample hops, the audition's exact integer crossfade and silence after the
932,885-sample ending. The build-generated schedule is checked in as
`assets/ending_focus_offsets.inc` and regenerated by the audition tool.
Source position search and waveform matching remain offline. Logical dictionary
seeks occur once per grain, with normal PCM reads handling slice boundaries.

`fib_stream_start_ending_focus()` starts this producer through the existing
four-buffer Paula transport after a caller binds Focus. Ending playback bypasses
the ordinary death fade; menu/retry stops it. This API requires the correct Focus
bank: it validates bounds but does not identify a track by its audio contents.
There is deliberately no automatic call from the live secret-ending controller.
The normal ending controller still needs to load/bind Focus and invoke this API
at its verified music transition. The F8 audition above provides explicit access
without claiming that either ending's automatic music integration is finished.

`venv/bin/python tools/audio/check_ending_stretch.py` compiles the production C
producer and compares all 932,885 output samples with the accepted audition.
It also exercises irregular read lengths, deliberately awkward dictionary slice
boundaries, silence after the end, and invalid source bounds. This passes, as
does the native link check. Real 68000 timing/underruns remain unmeasured.

`tests/fib_stream.c` feeds Paula channel 0 from four 512-byte Chip RAM buffers.
At the nominal 12,000 samples/second, each block lasts about 42.7 ms.
`fib_pcm.c` reads an already-decoded PCM dictionary through a logical slice
sequence. Fibonacci decoding is not performed per output sample. Channels 1–3
are allocated by `sfx.c` to effects.

This permits a different block producer without changing the DMA transport.
Overlapping grains need independent logical read cursors: physical bank slices
are not necessarily contiguous. Avoid calling the current linear-scan seek for
every sample; resolve starts once per grain, preferably in a build-generated
schedule. Crossfade the decoded, quantized Focus bank actually shipped, rather
than choosing alignments solely from the original high-resolution recording.

## Candidate implementations

| Method | Runtime work | Main limitation |
| --- | --- | --- |
| Lower Paula playback rate | Negligible | Changes pitch; not pitch-preserving stretching |
| Render ending offline, encode a new bank | Existing player only | Additional disk/cache space; compressed size must be measured |
| Offline-planned overlap-add | Read two grains and crossfade into existing buffers | Eightfold stretch can sound repetitive or phasey |
| Two Paula voices playing overlapping grains | Schedule DMA and volume envelopes | Takes an effects voice, requires Chip RAM staging and channel/panning management |
| Runtime WSOLA search | Correlate candidate overlaps, then mix | Adds avoidable CPU work for a fixed soundtrack |
| Runtime spectral stretching | Windowed transforms, spectral processing and overlap-add | Poor first choice for stock 68000 alongside rendering; not benchmarked |

Recommended first runtime experiment: offline-planned overlap-add. The build
chooses matching waveform boundaries; the Amiga follows a schedule. Start with
1,024- and 2,048-sample grains with 50% overlap. At eightfold stretch, a 1,024
grain has an output hop of 512 samples and a nominal source hop of 64 samples.
Samples within each grain still play at 12 kHz. Small offline alignment offsets
should stay near the intended timeline rather than accumulating timing drift.

Use complementary fixed-point windows, wide enough accumulation and final
rounding to signed 8-bit. A table can replace per-sample multiplies if profiling
justifies its memory cost. Mixing occurs in the buffer producer, not the audio
interrupt. Preserve the existing rule that DMA sees only completed buffers.

For a 168-second illustration, a 512-sample output hop needs about 3,938 grain
starts. Four-byte logical offsets occupy about 15.4 KiB before compression.
This excludes any extra metadata. Two 1,024-byte grain staging buffers would
add 2 KiB, plus window data and state; actual buffering design remains open.
The synthesis reads roughly 24,000 source samples per second at 50% overlap.
That is a workload estimate, not a measured CPU percentage or frame-rate claim.

A separate full 168-second 12 kHz mono 8-bit recording would contain 2,016,000
PCM bytes before dictionary/Fibonacci compression. It does not imply that many
bytes on disk, nor that the existing compressor will reach the tune budget.
Eightfold stretching of a 168-second output consumes about 21 seconds of source;
the correct Focus excerpt, direction, fades and possible further effects still
need identifying. Do not stretch the entire song and assume it matches.

## Validation before integration

1. Audition candidate Focus excerpts against the owned PC ending recordings.
   Compare ordinary slow playback, overlap-add and an offline spectral stretch;
   identify whether the desired sound needs spectral smearing rather than just
   retained pitch.
2. Generate the overlap schedule against the shipped Focus reconstruction and
   audition the exact proposed 8-bit output, including dictionary boundaries.
3. Benchmark its block producer with the target compiler/68000 cycle harness;
   then measure underruns and frame rate with the ending renderer and effects.
4. Check PAL/NTSC timing, bank cache selection, end/loop boundaries and Escape.
   Build/package only once the sound and runtime behaviour are acceptable.

No runtime audio or release assets are changed by this investigation.

## Sources

- [SoundTouch algorithm description](https://soundtouch.surina.net/README.html):
  time-domain WSOLA-like stretching, overlap/search parameters and distinction
  from sample-rate transposition. This supports the algorithm choice, not a
  claim that the library itself is suitable for a 68000.
- [SOLA explanation by the SoundTouch author](https://www.surina.net/article/time-and-pitch-scaling.html):
  overlapping sequences and waveform alignment to reduce join artifacts.

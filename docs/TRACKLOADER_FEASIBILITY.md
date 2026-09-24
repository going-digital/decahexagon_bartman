# Trackloader feasibility: first measurements

Measured using the local execram Zultra vendor library; independent Python zlib raw-DEFLATE decoding reproduced all six inputs byte-for-byte. Source revision and hashes are recorded in TRACKLOADER_PAYLOAD_MEASUREMENTS.json.

| Tune | Fibonacci + zultra | PCM + zultra | Decoded sample bank |
| --- | ---: | ---: | ---: |
| Courtesy | 229,869 | 301,501 | 490,050 |
| Focus | 242,049 | 340,607 | 494,246 |
| Otis | 231,646 | 299,905 | 490,887 |

Fibonacci payloads total 703,564 bytes. Estimated OFS allocation is 1465 sectors, leaving 295 sectors (151,040 bytes) BEFORE boot, root/bitmap, game, loader, saves and other assets.
PCM payloads total 942,013 bytes and need an estimated 1959 sectors: this path does not fit the 1,760-sector disk even without the game.

Recommendation: retain Fibonacci dictionaries, apply zultra to the compact binary package, inflate at loading time, then decode Fibonacci to the resident PCM bank before play. This preserves the audition samples; no quality reduction was made.

The TUN1 measurement header is provisional: binary sequence/offset metadata replaces audition JSON. Codec identity, checksums and the eventual disk container still need finalization. Sector figures include OFS data/header/extension blocks but are estimates, not a constructed image.

## Located dependencies

- Local checkout: `/Users/peterknight/dev/Github/execram`.
- Zultra upstream revision recorded by vendor README: `5490882fd561a8eae93c8004a46d11e641e46a0b`; raw DEFLATE framing, no preset dictionary.
- Adapted Keir Fraser inflate source: `stubs/inflate/inflate_core.s`, upstream revision `fdf7f28e6eb8e6084581df083d37d363052527fd`.
- Documented core ABI: A4 output, A5 input, A6 end of 2,928-byte scratch block; registers preserved.
- execram has a Musashi overlap-margin measurement implementation. Reuse/adapt its method for exact trackloader streams, then validate actual overlapping execution; these host round trips do not prove safe overlap.

## Remaining first-milestone work

1. Measure the game without embedded music, plus loader, SFX, cue and save allocations, against the remaining sectors.
2. Finalize and construct the filesystem image to replace sector estimates with actual allocation.
3. Exercise actual 68000 inflate with each stream, measure overlap slack and loading cost, then test Fibonacci expansion overlap separately.
4. Produce a live-allocation RAM map for boot and track replacement. The ~494 KB sample bank alone does not establish fit in 512 KB expansion: code, metadata and stacks also need placement.

Reproduce: `OPENBLAS_NUM_THREADS=1 venv/bin/python tools/trackloader/measure_payloads.py --execram-source /Users/peterknight/dev/Github/execram`.

No boot/runtime changes or final fit claims in this measurement pass.

## Game-without-music measurement

An isolated source copy under `scratchpad/trackloader/game_budget/` was built
with music off, SFX on and release flags. Root objects and release artifacts
were not changed. Its DOS executable is a sizing proxy, not the final boot game:

- Unpacked executable: 179,020 bytes.
- Resident HUNK allocations: 188,008 bytes (107,808 mandatory Chip).
- Zultra-packed executable: 86,236 bytes; compressed payload 84,593 bytes.
- Estimated OFS allocation: **180 sectors**.
- Tunes + this game consume an estimated 1,645 of 1,760 sectors, leaving
  **115 sectors / 58,880 bytes** before boot, root/bitmap, loader, additional
  music code/cues, saves and final format changes.

The packed executable self-check passed. Its OS startup wrapper is not suitable
for the boot loader and will be replaced; its size is neither a hard upper nor
lower bound on the final game. Full evidence: TRACKLOADER_GAME_MEASUREMENTS.json.

The earlier progress message's 60,416-byte remainder omitted three OFS header/
extension sectors. The accounting above includes them and supersedes it.

### Memory implication

The no-music game's ordinary hunks consume 80,200 bytes. The largest tune's
494,246 sample bytes leave only 30,042 bytes in 512 KiB expansion before even
sequence metadata. Thus the whole game and tune cannot all reside in expansion:
some ordinary code/data or a portion of the tune must occupy Chip RAM. The
existing split-bank PCM reader provides a useful basis for that arrangement.

A rough subtotal (188,008 resident game + 31,672 display/sprite runtime bytes
+ 19,712 disk workspace + 494,246 largest sample bank + 2,048 PCM DMA buffers
+ 2,928 inflate scratch) is 738,614 bytes. It excludes music code, metadata,
loader/vectors/stacks, inflate overlap slack and any extra Fibonacci input
staging. Keeping another ~250 KB Fibonacci bank simultaneously pushes that
subtotal close to 1 MiB before those exclusions. This reinforces the need to
prove an overlapping Fibonacci expansion or a bounded decode/staging schedule.
It is not a final RAM-fit claim. Re-measure allocations from the integrated
link map rather than treating historical display figures as authoritative.

Next concrete gate: execute the exact inflate core on the tune streams with
read/write overlap instrumentation, then validate a bounded Fibonacci expansion
layout. Finalize filesystem construction only after those buffer lifetimes are
known.

## Actual 68000 inflate probe: compatibility issue found

Added a standalone Musashi harness that assembles the local adapted Keir
inflate core, supplies its documented scratch/ABI, and compares decoded bytes
against independent zlib output. Each successful disjoint run measures maximum
output-minus-input progress and is replayed with genuinely overlapping buffers.
Boundary canaries are checked. Sources and results are in
`tools/trackloader/{build_inflate_probe.py,inflate_probe.c,run_inflate_probe.py}`
and `TRACKLOADER_INFLATE_RESULTS.json`.

Results for the intended Fibonacci payloads:

| Tune | Disjoint decode | Overlap replay | Extra tail bytes | PAL CPU-only time |
| --- | --- | --- | ---: | ---: |
| Courtesy | matches | matches | 2 | 7.13 s |
| Focus | matches | matches | 0 | 7.33 s |
| Otis | **mismatch** | not attempted | unknown | not validated |

Otis differs first at output byte 36,065. The larger PCM alternatives also
expose mismatches for Courtesy and Focus; Otis PCM passes. Stored, random,
repetitive, mixed and tiny synthetic controls pass. A failing disjoint decode
is not an overlap failure. The issue could be in the adapted decoder or harness;
its cause has not yet been isolated. Do not integrate this core as verified or
claim a universal overlap bound from the successful cases.

The matching Fibonacci streams already take over seven seconds in the CPU-only
68000 model, before disk seeks, DMA contention and Fibonacci expansion. Loading
UX needs to account for this; those figures are lower bounds, not measured
A500 wall-clock times.

Next action: isolate the Otis mismatch, compare the adapted core against the
pinned original, and retest every stream before continuing loader integration.
Then prove the second (Fibonacci-to-PCM) expansion's memory layout separately.

Reproduce after `measure_payloads.py`:

```sh
python3 tools/trackloader/build_inflate_probe.py --execram-source /Users/peterknight/dev/Github/execram
python3 tools/trackloader/run_inflate_probe.py
```

The runner records failures alongside successes so all cases are inspected;
read the report before interpreting completion as a pass.

## Maximum-distance defect isolated and fixed in the probe

The preceding mismatch is resolved. Both the local adapted core and the local
original-source copy use `sub.w d0,a0` to calculate a match address. On 68000
this is SUBA.W and sign-extends the operand. A DEFLATE distance of 32,768 is
therefore interpreted as -32,768, reading 65,536 bytes ahead of the intended
match source. This is not an overlapping-buffer issue.

`build_inflate_probe.py` now generates a local core copy replacing that one
instruction with `and.l #$ffff,d0` followed by `sub.l d0,a0`. The external
execram checkout is unchanged; the build asserts a unique patch site. Its
`--unpatched` option reproduces the original core for comparison.

An independently constructed fixed-Huffman stream contains 32,768 literal
bytes followed by a 258-byte match at distance 32,768. The original core fails
at byte 32,768; the corrected core matches independent zlib output and passes
overlap replay. Evidence: TRACKLOADER_INFLATE_REGRESSION.json. All six actual
tune streams now match, including the three previously failing streams.

For the intended Fibonacci packages, measured extra tail allocation is 2 bytes
for Courtesy, 0 for Focus and 2 for Otis, after rounding source placement to an
even address. These are per-stream measurements tied to the recorded hashes,
not a universal margin. Inflate scratch remains separately allocated. The
second Fibonacci-to-PCM expansion still needs its own proof and tests.

The control suite includes stored, random, repeated, mixed, tiny and explicit
maximum-distance streams. The runner writes all failures to the report and now
exits nonzero if any fail. CPU timing still excludes disk and chipset contention.
No game runtime or external compression tool was modified in this fix.

## Fibonacci expansion: bounded in-place implementation

Added `trackloader/fib_expand.c`, a loading-time FIB1 expander using the existing
`fib_decode_block` implementation. It validates magic, block size, sample count,
capacity and exact encoded length before modifying the arena. Each block has an
independent predictor, so blocks can be processed from last to first. Stage one
encoded block (at most 257 bytes), then decode it forward into its final PCM
position. No second full compressed bank is needed.

For block k >= 1, destination start k*512 is at or beyond source start
12+k*257. Staging the current block therefore prevents output from overwriting
its own input, and output cannot touch earlier unread blocks. Block zero has
no earlier input; the file header has already been validated and consumed.
The 257-byte scratch buffer is on the stack and must be included in stack sizing,
not counted as the entire function's stack requirement.

Host execution of the actual C decoder passes all three full banks byte-for-byte
against the independent Python decoder, plus 1,031 synthetic lengths with arbitrary
codes (wraparound, odd tails and block boundaries). Six malformed cases fail
without changing the buffer. Guard bytes remain intact. See
`TRACKLOADER_FIB_EXPANSION_RESULTS.json`. Reproduce with:

```sh
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/trackloader/check_fib_expand.py
```

Proposed phase layout, keeping binary tune metadata at the arena's start:

1. Place the DEFLATE input at its measured offset and inflate the binary
   metadata + Fibonacci package into the same arena.
2. Finish inflate completely, preserving metadata. Expand the following FIB1
   bank in-place from its last block to its first.
3. Apply the agreed PCM gain once, validate/bind the sequence, and release
   loading scratch before starting playback.

Metadata plus final sample-bank arena sizes are 494,146 bytes (Courtesy),
496,646 (Focus), and 492,699 (Otis). These exceed each measured inflate-phase
arena, so the final allocation can cover both phases without additional tail
allocation for these exact streams. Inflate scratch and CPU stacks remain
separate; guard allowances and final payload/container headers are not included.

This removes the roughly 250 KB second-bank concern from the proposed RAM
layout. These are host tests of the C expansion, not target timing or a combined
68000 inflate/expand run. Next: exercise the combined loading path on 68000,
measure expansion time/stack high-water, and allocate the contiguous music arena
in expansion RAM while placing resident code/data in remaining Chip RAM as needed.

## Combined 68000 loading path verified

The disk sources now live at project root: `DiskIO.s`, `DosIO.s`, `DosIOR.s`.
`make test-diskio-disassembly` still passes; documentation moved to
`docs/DISKIO_DISASSEMBLY.md`. Include paths and the implementation plan were
updated to match.

`check_combined_loading.py` compiles the actual C Fibonacci expander and existing
block decoder for 68000, then runs them in Musashi immediately after the patched
inflate core's overlapping replay. The inflated package is not copied elsewhere.
Binary metadata remains intact, the FIB1 bank expands at its existing address,
and the entire final metadata+PCM image is compared with the independent result.
Arena guard bytes are checked. All three tunes pass.

| Tune | Shared arena | Inflate cycles | Expansion cycles | CPU-only PAL seconds |
| --- | ---: | ---: | ---: | ---: |
| Courtesy | 494,146 | 50,586,896 | 33,307,706 | 11.83 |
| Focus | 496,646 | 52,017,582 | 33,592,790 | 12.07 |
| Otis | 492,699 | 51,023,182 | 33,364,268 | 11.90 |

The expansion stack write high-water is 348 bytes below the entry SP in this
build (including its scratch buffer and nested calls). This is not a complete
stack reservation: caller frames, arguments and any enabled interrupts need
additional allowance. Inflate's separate 2,928-byte scratch remains required.

These tests execute actual 68000 code but model no disk latency, DMA contention
or OS boot behaviour. The times exclude PCM gain conversion, checksums and
file I/O. Expect actual track switches to take longer; retain the loaded bank
for retries and same-tune selections. Music gain still must be applied exactly
once when binding the decoded bank.

Reproduce after building the inflate harness:

```sh
python3 tools/trackloader/check_combined_loading.py
```

Evidence: `TRACKLOADER_COMBINED_RESULTS.json` includes the expansion binary hash.
The next milestone is the filesystem read/write harness and deterministic ADF
layout, followed by the resident boot/platform layer. The full disk and linked
memory layout still need validation; these checks close the combined decoder
correctness question for the measured tune payloads, not the entire boot plan.

## Filesystem harness and first populated ADF

The actual root-level `DosIO.s` now runs under Musashi to format, save and load
files. Only calls entering its included DiskIO entry are intercepted and backed
by a 1,760-sector memory image. This tests the original 68000 filesystem logic,
including its allocator, block lists, checksums and file overwrite behaviour;
it does not exercise CIA timers, raw MFM, physical drive or disk DMA.

The generated `scratchpad/trackloader/filesystem_trial.adf` contains all three
zultra-compressed Fibonacci tunes, the no-music DOS-game sizing proxy and two
512-byte test save files. Every save is loaded back with exact length/content
comparison and a destination overrun guard. One save file is overwritten with
a different pattern and verified. Root and bitmap checksums pass. The actual
bitmap reports **105 free sectors / 53,760 bytes**, after filesystem metadata
and these save files. Results: `TRACKLOADER_FILESYSTEM_RESULTS.json`.

Reproduce after building the Musashi inflate harness and measured payloads:

```sh
python3 tools/trackloader/check_dosio_filesystem.py --execram-source /Users/peterknight/dev/Github/execram
```

This is a filesystem trial image, **not a bootable trackloader release**. It
has no custom boot/resident stage. The game remains the previous size proxy;
the remaining sectors must accommodate platform/loader changes and music cues.
Save test files are not yet guaranteed to occupy separate physical tracks and
ordinary DosIO overwrite is not the proposed crash-safe save implementation.
Disk-change/write-protection/failure injection, independent filesystem traversal,
and target DMA checks remain open.

Next: audit/validate sector ownership and reserve bootstrap plus separated save
tracks; then build a minimal target harness that runs these reads through real
DiskIO in FS-UAE/Copperline before implementing the permanent OS takeover path.

## Independent layout audit and raw-sector reservations

`check_adf_layout.py` independently traverses root hash buckets, file headers,
extensions and data chains. It verifies checksums, exact file contents, lengths,
unique block ownership, and equality between allocated blocks and bitmap state.
Two deliberately corrupted blocks are rejected. The validator follows the
supplied writer's convention: data-block header keys reference their containing
header/extension block; extension parents retain the root directory reference.
This checks the generated DosIO layout, not arbitrary Amiga filesystem variants.

The audit finds 105 free sectors, including wholly free physical tracks 151–159.
The two original test save files share a track, so they are unsuitable as the
persistent redundant pair.

`reserve_disk_layout.py` creates `scratchpad/trackloader/reserved_layout.adf`:

- Tracks 151–154: provisional 22,528-byte raw bootstrap region.
- Track 158: persistent slot A, isolated at track granularity.
- Track 159: persistent slot B, isolated from slot A and immutable files.
- All reserved sectors are marked unavailable in the allocation bitmap, with
  its checksum recomputed. Only the bitmap changes; all existing files and
  directory blocks remain byte-identical. Independent traversal passes again
  with the explicit reservation manifest accounting for the raw sectors.
- Remaining unreserved space: 39 sectors / 19,968 bytes.

This is a proposed raw-region layout, not a bootable release or a save journal.
The bootstrap reservation is an allowance, not a measured stage binary size.
The old test save files remain only as harness fixtures. Build tools must retain
and validate the raw reservation manifest; general filesystem repair tools do
not know these intentionally allocated non-file sectors.

Reports: `TRACKLOADER_DISK_LAYOUT.json`, `TRACKLOADER_RESERVED_LAYOUT.json`.
Run the audit and reservation scripts after `check_dosio_filesystem.py`.
Next: target execution using the real DiskIO DMA/CIA code on a disposable image,
then resident bootstrap and bounded raw-slot persistence.

## First real disk-DMA boot test passes

Added a diagnostic boot block and resident stage under `trackloader/tests/`.
`build_disk_boot.py` constructs a disposable `disk_dma_trial.adf` from the
reserved layout. Boot allocates 320 KiB Chip RAM and reads the reserved stage
using the boot IORequest, then enters supervisor mode. The stage disables OS
interrupts/DMA ownership and calls no OS services thereafter.

Copperline A500OCS PAL / Kickstart 1.3 cold boot produced `TRACKDISK-PASS`:

- Actual DosIO and DiskIO loaded the 242,049-byte Focus payload after takeover;
  target length and additive byte checksum matched.
- Actual DiskIO wrote a $5aa5 pattern to sector 1738 in reserved save track A,
  cleared its RAM buffer, read the sector again and compared every word.
- Host inspection after emulator exit confirmed the write persisted. Exactly
  sector 1738 changed relative to the constructed boot image; no file, bitmap,
  bootstrap or other save-track sector changed.
- Resident diagnostic stage is 3,914 bytes, within its 22,528-byte reservation.

Evidence: `TRACKLOADER_DMA_RESULTS.json`; local serial log and screenshot are
`scratchpad/trackloader/disk_dma.log` and `disk_dma.png`. Rebuilding the diagnostic
ADF deliberately resets this disposable test disk, not the source reserved ADF.

This is not the integrated game. Only one PAL emulator has been exercised;
NTSC/FS-UAE, error handling and interrupted-write persistence remain open. The
bootstrap uses minimal Exec allocation and boot-device reads before takeover,
not a zero-OS first stage. The next work is integrating the verified loading
pipeline and memory arenas, then transactional saves and broader target checks.

## Combined cold-boot disk-to-PCM diagnostic passes

`build_load_boot.py` adds a separate read-only diagnostic, preserving the earlier
write/readback disk test. It assembles a position-independent 68000 C expander
and includes the corrected inflate core in a 5,640-byte resident stage.

A Copperline A500OCS PAL run with explicit 512K Chip + 512K slow RAM and Kickstart
1.3 emitted `TRACKLOAD-PASS`. After permanent OS takeover, the stage loads Focus
through real DosIO/DiskIO into the measured overlapping input position, inflates
metadata plus FIB1, then expands the bank in place. Packed length/checksum,
expansion return length, and final metadata+PCM additive checksum pass. This
checksum is weaker than the byte-for-byte checks in the prior independent
harnesses; no target full-hash comparison is claimed.

The initial attempt to allocate a separate 500,000-byte arena before takeover
failed to reach the stage. A subsequent instrumented boot identified that a
presence check beginning at $c00000 also fails: the expansion memory header is
not caller-addressable memory according to TypeOfMem. The diagnostic now starts
at $c01000, checks each required page through $c7a000, installs its private Chip
stack and disables OS interrupts before writing expansion RAM. Its arena is
496,646 bytes. This is an A500-specific diagnostic reclaim strategy, not yet a
general production allocator or proof of accelerator compatibility.

The boot stage and stack use a separately allocated 64K Chip block. Disk
workspace is at +24,000, inflate scratch ends at +52,000, and stack starts at
+65,536. The phases do not overlap those working regions. Expansion RAM is only
reclaimed after takeover and no OS return is attempted.

Evidence: `TRACKLOADER_LOAD_BOOT_RESULTS.json`; local image/log/screenshot are
`scratchpad/trackloader/load_trial.{adf,log,png}`. Reproduce by building the prior
inflate harness, then running `python3 tools/trackloader/build_load_boot.py`.
The test used a write-protected disposable ADF. No target load-time measurement,
audio playback, other-track target run, NTSC or FS-UAE result is claimed yet.

Next: replace diagnostic constants with validated package metadata and memory
map contracts, then connect the dynamic PCM player/game platform to the resident
loader. Preserve the existing DOS build while integrating this path.

## Validated tune loading and existing PCM sequencer binding

Added `trackloader/tune.c` and `tune.h`. The check API validates the inflated
TUN1 package before writes: format/rate, bounded dictionary and sequence counts,
monotonic offsets, slice bounds/durations, total logical length, FIB1 header,
exact encoded size and final arena capacity. The prepare API expands in-place
and binds the existing `PcmSong` reader directly to metadata and PCM; no second
bank or per-frame decompression is introduced. It leaves gain unchanged so the
future loading caller must apply the agreed curve exactly once before playback.

`check_tune.py` tests actual C expansion plus the existing PCM reader for all
three tunes. Final arena bytes match independent decoding. Full-song playback
plus 4,096 looped samples and four seeks per tune match the reference sequence;
output arena guards survive. Fourteen malformed/truncated/undersized cases per
tune are rejected without changing the package buffer. Evidence:
`TRACKLOADER_TUNE_RESULTS.json`.

```sh
python3 tools/trackloader/check_tune.py
```

This validates metadata after inflate; it is not a bounds-checking DEFLATE
wrapper. The resident loader must still validate trusted outer file sizes,
checksums and overlap placement before entering inflate. The diagnostic boot
still has fixed Focus constants, and the game still embeds its original bank:
this new API is not yet wired into target level transitions or Paula playback.
Next: cross-compile this API into the resident stage, replace its per-tune
constants with a validated outer manifest, and connect the player's stopped-state
bank-switch interface and per-track cue tables.

## Validated resident API passes all three tune cold boots

The resident diagnostic now calls the cross-compiled `trackloader_tune_prepare`
API rather than the bare Fibonacci expander. It validates inflated metadata,
expands PCM and initializes the existing PcmSong structure. The builder accepts
`--track courtesy|focus|otis` and generates filename, lengths, checksums and
source placement from each fixture. It checks the compressed SHA-256 against
the corresponding successful overlap measurement and refuses a stale layout.
These are trusted build-time manifest constants, not yet an on-disk outer
container or arbitrary-file safety mechanism.

Three separate write-protected Copperline cold boots passed on PAL A500OCS with
512K Chip + 512K slow RAM: Courtesy, Focus and Otis each emitted TRACKLOAD-PASS.
Each disk remained byte-identical after execution. Stage sizes are 6,886 bytes
for Courtesy and 6,882 for Focus/Otis, comfortably below the reserved size.
Host full-song/loop/seek and malformed-input checks also still pass, as do disk
assembly byte-identity checks.

Evidence: `TRACKLOADER_ALL_TUNES_BOOT_RESULTS.json`. Reproduce with:

```sh
python3 tools/trackloader/run_load_trials.py
```

The runner builds one diagnostic per tune and keeps separate images/logs/screenshots
under `scratchpad/trackloader/load_<track>.*`. These are three separate boots,
not repeated switching in one running game. The PcmSong binding is exercised
on target; Paula playback, gain conversion, per-track cue binding, level-change
transitions and the production boot game remain the next integration work.
The diagnostic still loads through unbounded historical DosIO before checking
returned file length, so corrupt-filesystem safety needs the planned bounded
sector/manifest loader before production use.

## PCM player stopped-state bank interface

`tests/fib_stream.c/.h` now expose `fib_stream_bind` / `fib_stream_unbind`.
The loader can supply the validated PcmSong, logical sample count, per-track cue
pointer/length and an even-addressed 2,048-byte Chip DMA buffer allocation.
Bindings are rejected while playback or buffer ownership is active. The caller
must stop first and keep all bound storage alive until unbinding. The player
copies sequencer state, so retry playback starts with a fresh cursor.

Bound playback uses loader-owned buffers without AllocMem/FreeMem. Stop disables
audio IRQ/DMA, detaches the handler and clears the active buffer pointer, but
does not free externally owned memory. The existing embedded Courtesy path
retains its allocation/free behaviour. Unbinding returns to that fallback.
Cue lookup uses the active bank's explicit pointer/length; missing external cues
produce zero instead of reading Courtesy data. Only music1.cues is currently
present: other tracks' cue tables still need extraction/verification.

External banks currently start at sample zero. Courtesy's embedded retry-offset
behaviour is preserved, but external per-track retry offsets remain pending.
The binder expects validated, already gain-adjusted PCM; it neither revalidates
the entire song nor applies gain. Loading remains outside the player/IRQ loop.

Validation: `make test-pcm-binding test-pcm-lifecycle` passes. The binding test
executes the actual bind/unbind/stop functions with mocked hardware, checks
active-state rejection, cue selection state, DMA shutdown and external/embedded
buffer lifetime. The updated player compiled successfully with the 68000 release
toolchain in the isolated diagnostic build. This is not a target audible playback
test; the game's level transitions still do not invoke the binding API.

Next: supply a caller-owned Chip buffer and prepared bank from the resident
harness to actual Paula playback, then wire level-change loading and recoverable
errors into the game. Production removal of embedded assets/OS allocations is
still separate platform-integration work.

## First decoded-tune Paula output captured

Added an optional `--paula` resident diagnostic. After the validated load and
full decoded checksum, it applies the same offline-derived gain lookup once to
the sample bank, then uses `fib_song_read` and two 512-byte Chip buffers to feed
AUD0. It polls completion with a bounded timeout, uses PAL period 296 and volume
64, completes 128 buffer transitions after priming, then stops DMA. This isolates
Paula output under permanent takeover; it is not the game's IRQ-driven player.

A cold boot of Focus in Copperline (PAL A500OCS 512K Chip + 512K slow) passed and
produced a captured 44.1 kHz stereo WAV. The tune is audible-data activity around
seconds 43–48.44 of the boot capture, after silence through seconds 35–42. Earlier
capture activity includes boot/drive sounds and is not counted as music proof.
A short excerpt is `scratchpad/trackloader/paula_focus_excerpt.wav` (43–49 s).
The checks establish finite, nonzero recorded output and completed DMA polling;
no reference-waveform match, underrun-free IRQ behaviour or listening acceptance
is claimed. Evidence: `TRACKLOADER_PAULA_RESULTS.json`.

Build with `python3 tools/trackloader/build_load_boot.py --track focus --paula`.
The stage remains below its reservation (9,244 bytes in this trial). The diagnostic
uses caller-owned buffers at stage+56,000; it has no gameplay allocations or OS
calls after takeover. Other tracks and NTSC remain untested for this audio path.
Next is switching the resident caller to the game's IRQ-driven player and wiring
level changes, cue selection and loading-state clock resets into game startup.

## External-only IRQ player build mode

Added `PCM_EXTERNAL_ONLY=1` alongside `MUSIC_FIB_STREAM=1`. It removes embedded
PCM and Courtesy cue assets, embedded-asset build prerequisites, and allocation/
free calls from the player. Startup without a loader-bound bank fails visibly
through the diagnostic counter instead of falling back to embedded data.
The default DOS/embedded path remains available. Build modes still share the
legacy obj directory: use isolated builds or force rebuilding when switching.

`check_external_player.py` cross-compiles the actual IRQ player and audits all
undefined symbols. They are exactly custom/frameCounter/video_timing, Paula IRQ
registration, PCM read/seek, pulse position helpers and lifecycle tick. There is
no SysBase/Exec allocation dependency or embedded-bank dependency in this object.
Evidence: `TRACKLOADER_EXTERNAL_PLAYER_RESULTS.json`.

Binding/stop tests now cover both embedded-capable and external-only modes;
lifecycle/seek checks pass. Added `fib_stream_status` exposing underrun and block
counters for the next target diagnostic (individual atomic snapshots, not an
atomic pair). The external player is not yet linked into the cold-boot stage:
that stage still uses the prior polling test. Next is wiring resident hardware
state, level-3 VBlank and level-4 audio dispatch to this exact IRQ player, then
checking sustained playback and stopped-bank replacement on target.

## Actual IRQ-driven player cold-boot test passes

Added `trackloader/tests/irq_probe.c` and `build_load_boot.py --irq`. The resident
stage now optionally links the actual external-only `tests/fib_stream.c`,
`paula_irq.c`, lifecycle and video timing code. The diagnostic supplies hardware
globals and a VBlank handler that advances frameCounter and fills free buffers;
the game's level-4 audio handler drives Paula buffer handoff unchanged.

Copperline PAL A500OCS (512K Chip + 512K slow, Kickstart 1.3) passed after loading
Focus and applying gain once: 300 VBlanks of IRQ-driven playback, zero underruns,
at least 130 completed blocks, successful stop and unbind. The captured WAV is
finite and nonzero; a short excerpt is `scratchpad/trackloader/irq_focus_excerpt.wav`.
Evidence: `TRACKLOADER_IRQ_PLAYER_RESULTS.json`. No waveform fidelity or
perceptual acceptance claim is made.

Build: `python3 tools/trackloader/build_load_boot.py --track focus --irq`.
The resident diagnostic occupies 20,644 disk bytes. Its linked BSS is also
accounted for: a build assertion prevents code/data/BSS reaching the disk
workspace at stage+24,000. It fits the bootstrap disk reservation, though this
is a diagnostic link with alignment padding, not the final game layout.

This test has no game rendering or level transitions and supplies no Focus
visual cues. It is one PAL emulator run, not a full-song or NTSC check. Existing
binding/lifecycle/seek tests continue to pass. Next: stop/rebind/restart within
one boot, then connect resident loading to actual game selection and loading
states, preserving cue clocks and adding recoverable disk errors.

## IRQ player stop, unbind and restart test passes

The `--irq` diagnostic now plays the same Focus bank twice in one boot. Each
run lasts 150 PAL VBlanks and requires zero underruns and at least 65 completed
blocks. It checks that bind/unbind requests during playback are rejected, then
stops, unbinds, verifies AUD0 DMA is disabled and checks that counters remain
unchanged during 50 idle VBlanks. Before each bind it overwrites the retired
DMA buffers with 0x55 to exercise buffer initialization on restart.

Copperline PAL A500OCS (512K Chip + 512K slow) reports `TRACKLOAD-PASS`.
The recording contains two approximately three-second playback intervals at
43–46 and 47–50 seconds, separated by a silent gap. The excerpt is
`scratchpad/trackloader/irq_restart_excerpt.wav`; structured evidence is
`TRACKLOADER_IRQ_RESTART_RESULTS.json`. The stage occupies 20,836 bytes and
passes the existing disk-reservation and workspace-boundary assertions.

This establishes same-bank player restart under takeover. It does not yet test
loading a different track between runs, gameplay rendering, NTSC or other
emulators. Next is different-track disk loading within one boot, followed by
actual level-selection integration, cue data and recoverable loading errors.

## Different-track loading and playback in one boot passes

`build_load_boot.py --track focus --irq --switch` now runs Focus, Courtesy and
Otis sequentially under permanent takeover. A generated descriptor table carries
each filename, measured overlap offset, lengths and checksums. The stage reuses
the same tune arena, validates each packed load and expanded bank, then calls the
actual IRQ player diagnostic. Each track plays twice with stop/unbind/restart;
the player shuts down before DosIO overwrites the arena for the next track.

Copperline PAL A500OCS (512K Chip + 512K slow) completed the entire sequence with
`TRACKLOAD-PASS`: six playback runs, zero underruns and at least 65 completed
blocks per run. The stage occupies 20,988 bytes. Host binding, lifecycle and seek
tests also pass, and the single-track build remains available. Evidence is in
`TRACKLOADER_IRQ_SWITCH_RESULTS.json`; the full audio capture is
`scratchpad/trackloader/irq_switch.wav` (no listening or fidelity claim).

This closes the diagnostic gap between same-bank restart and different-track
disk loading. Actual game level-selection/loading states, cue data, recoverable
disk errors, save journalling and broader hardware/video validation remain.

## Game run-start preparation boundary

The game now exposes `game_set_run_preparer`, an optional synchronous platform
callback installed after `game_init`. Both menu confirmation and death-screen
retry call it with the selected profile before resetting the run, emitting start
SFX or entering play. Failure preserves the prior mode; confirm must be released
before another attempt, preventing repeated disk loads from a held retry button.
The platform owns bank reuse, stop/unbind, loading, validation and binding, and
must return success only once the selected audio is ready.

After any preparation attempt, `game_take_load_barrier` tells the main loop to
discard remaining catch-up ticks, clear the fractional simulation clock and
rebase its VBlank timestamp. Disk/decode time therefore cannot advance the new
run on the next frame. No callback is installed by the existing DOS entry point,
so it retains immediate startup.

`make test-game-loading` exercises the actual start-boundary functions for all
six profiles, failed loads, held retries, successful starts and default startup,
with cheat input enabled and disabled. Both changed translation units also
cross-compile for the default and external-player configurations. This is not
an emulator test of game loading: the resident diagnostic and game entry point
are still separate. The next prerequisite is a boot-native platform entry with
reserved display/audio memory instead of the current Exec allocations, followed
by installing the loader callback and displaying loading/error states. Profile
track mapping and cue/retry data also need verification before that integration.

## Native game entry links without OS dependencies

Added the release-only `TRACKLOADER=1` platform path and
`trackloader_game_entry(const TrackGameBoot *)`. The resident caller supplies
an exclusive Chip heap, VBR, PAL/NTSC selection and the run-preparation callback.
Native startup opens no libraries, performs no Exec allocations and does not
attempt to restore AmigaOS on exit. It enables CPU interrupts after installing
the game handlers, and returns with interrupts/DMA off. The caller must enter
in supervisor mode with interrupts masked and hardware quiesced, relocate the
image, zero its BSS and provide disjoint live memory regions. This is currently
an uncached 68000 contract, not accelerated-machine support.

Display, HUD and renderer allocations now use a common cleared-Chip interface.
DOS builds retain Exec allocation/free; native builds use `chip_arena.c`, a
bounded, eight-byte-aligned allocator with arbitrary-order free and adjacent
block coalescing. `make test-chip-arena` checks exhaustion, overflow, alignment,
clearing, fragmentation/reuse and arena-edge guards. Those tests and the existing
loading/binding/lifecycle/seek tests pass. An optional AddressSanitizer run
stalled both inside and outside the sandbox and was stopped; no sanitizer pass
is claimed. Changed DOS translation units cross-compile as well.

`python3 tools/trackloader/check_native_game.py` links the actual game, external
music player and SFX. The result has no undefined imports and no SysBase,
DOSBase, GfxBase, DOS `_start`, `main` or KPrintF symbols. Section evidence is in
`TRACKLOADER_NATIVE_GAME_RESULTS.json`. This build is an isolated link audit;
it does not modify the release ADF or create a playable native ADF.

Next: implement the resident executable load/relocation and final memory layout,
then boot this entry on target. Static DMA assets must be in Chip memory (the
current candidate contract places the whole image there); heap, tune arena,
loader workspace and stack must not overlap. The existing diagnostic soundtrack
loader still needs a callable adapter installed through `boot->prepare`, with
verified profile mapping, loading/error UI and playback timing validation.

## Executable packaging and relocation verified on 68000

The native link now retains relocation records. `package_native_game.py` builds
the provisional EXE1 format documented in `TRACKLOADER_EXECUTABLE_FORMAT.md`:
a checked header, sorted address fixups and initialized image. The current game
has 1,004 fixups, a 169,189-byte package, 184,012-byte resident footprint including
BSS, and an 83,712-byte raw zultra stream. This excludes the display heap,
soundtrack arena and resident loader/workspace/stack.

`track_executable_prepare` validates the complete package and CRC before writes,
relocates addresses in place, moves the image over the header/table and clears
BSS. Host execution of the actual C matches independently linked executables
at 0x20000 and 0x40000 byte-for-byte, with 19 malformed cases rejected without
mutating the arena or output descriptor. Actual 68000 execution in Musashi also
matches both independent images and rejects checksum damage. A 64-byte CRC
table reduces preparation from 131,618,480 to 46,719,688 cycles (about 6.6 seconds
at PAL A500 clock, excluding chipset contention).

The actual corrected inflate core separately reproduces this package in place:
packed source offset 85,478, arena 169,190 bytes, 28,978,732 cycles (about 4.1
seconds CPU-only). The resident image allocation covers both inflate and final
relocation/BSS phases. These measurements are tied to the recorded payload hash.
Evidence: `TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json`,
`TRACKLOADER_EXECUTABLE_RESULTS.json`, and
`TRACKLOADER_EXECUTABLE_TARGET_RESULTS.json`.

This completes executable format/relocation proof, not a playable native ADF.
Next is placing the compressed native package in the filesystem and wiring the
bootstrap to load, validate, inflate, relocate and call its entry with disjoint
Chip heap and loader resources. Full-game emulator boot and soundtrack callback
integration remain untested.

## Actual native game menu boots from the filesystem

`python3 tools/trackloader/build_native_boot.py` now creates a disposable native
menu ADF from the verified executable package. It formats a fresh filesystem
with the real DosIO harness, stores all three tunes and the native game, verifies
every file and sector owner independently, and reserves the existing bootstrap
and separate save tracks. The generated image has 44 free sectors; save files
are still fixtures and no journal is implemented.

The boot block allocates one 360,448-byte Chip block before permanent takeover.
Its regions are stage/workspace/stack [0,65536), executable arena
[65536,262144), and display heap [262144,360448). The 5,834-byte resident stage
loads `DF0:game`, checks packed length and byte sum, inflates with the measured
overlap placement, validates EXE1/CRC, relocates and clears BSS, then calls the
actual native game entry. It quiesces CIA/custom interrupts and DMA before
handing control to the game. This remains an A500 PAL diagnostic boot contract.

Copperline PAL A500OCS with 512K Chip + 512K slow and Kickstart 1.3 successfully
booted this image. Serial output reached `NATIVE-GAME-ENTRY`, and visual
inspection of the 100-second screenshot confirms the Hexagon menu, HUD and
playfield rendered by the actual game. The disk hash remained unchanged.
Evidence: `TRACKLOADER_NATIVE_BOOT_RESULTS.json` and
`scratchpad/trackloader/native_game/native_menu.png`. The ADF is
`scratchpad/trackloader/native_game/native_menu.adf`; release images are untouched.

This is a native menu launch, not playable soundtrack integration. The current
preparation callback explicitly rejects starting a run until its loader adapter
is installed. Next: expose callable resident tune loading to that callback,
establish profile-to-track/cue mapping, and test real runs and level changes.
Disk-error recovery, persistence, NTSC and accelerator validation remain open.

## First playable native soundtrack integration: Courtesy

The native preparation callback now accepts Hexagon and Hyper Hexagon (profiles
0 and 3). The PC restart code in `scratchpad/decompile_gameinput_full.txt`, lines
1556–1630, selects tracks 1/2/3 from base stage 0/1/2 before branching on Hyper;
track 1's Courtesy identity/alignment is recorded in `soundtrack/pc_cues.json`.
Other profiles remain deliberately blocked pending their track/cue integration.

For the first accepted run, the resident callback stops/unbinds music, stops
SFX, masks interrupts and pauses display DMA, loads Courtesy into the existing
slow-RAM tune arena, verifies its packed size/sum, inflates, validates/expands
TUN1, applies gain once and binds the game's actual IRQ player. It supplies the
verified 11,441-byte Courtesy cue table and resident Chip DMA buffers. Hardware
state is restored before the game starts. SFX interrupt callbacks are explicitly
reinstalled after loading; merely stopping SFX had detached those callbacks.
Subsequent accepted runs reuse the loaded bank without reloading or applying gain
again. External-player retries still begin at sample zero.

Native bind/unbind symbols are retained in the game link for resident calls.
The builder checks that the package and ELF hashes match before using exported
function offsets. The updated executable is 84,030 packed bytes; the resident
stage including cues is 20,311 bytes, below the 22,528-byte reservation. The
filesystem has 43 free sectors. Executable host/68000 relocation and overlap
checks were rerun for this payload.

Copperline PAL A500OCS 512K Chip + 512K slow passed a real start-and-retry test:
Space at 60 seconds initiated the first load, audible output began around 95
seconds, and Space at 115 seconds restarted immediately. Exactly one
`NATIVE-TUNE-READY` message was emitted. Both runs produced finite, nonzero audio;
the final screenshot shows 8.73 seconds of gameplay in the second run. The disk
hash remained unchanged. This confirms short gameplay/audio integration, not
full-song fidelity, long-run stability or zero underruns under rendering.
Host loading/binding/lifecycle/seek/allocator tests pass.

Evidence: `TRACKLOADER_NATIVE_COURTESY_RESULTS.json`,
`scratchpad/trackloader/native_game/courtesy_final.png`, and
`scratchpad/trackloader/native_game/courtesy_retry_excerpt.wav`.
The diagnostic ADF remains `scratchpad/trackloader/native_game/native_menu.adf`.
Hyper eligibility was not bypassed or runtime-tested. Next: verify the other
track identities/cues and integrate their selection, then loading/error UI,
retry timing, longer gameplay/underrun checks and persistence.


## Gain baked in before Fibonacci encoding

The original dictionary PCM is now boosted with the existing soft-knee curve
before optimal Fibonacci encoding. Dictionary selection, 12 kHz rate and the
250 KiB tune budget are unchanged. New audition assets live in
`scratchpad/audio/codec_audition_budget_250_preboost`; earlier encodes remain
available as controls. Decoded previews receive no second boost. The resident
loader and polling/IRQ diagnostics no longer apply a gain lookup.

Regenerated raw-zultra Fibonacci sizes are Courtesy 232,835 bytes, Focus 229,616
and Otis 225,897: 688,348 total, 15,216 fewer than the preceding set. All three
experimental AUD packages remain under 250 KiB. Actual-C tune validation,
complete sequencer loops/seeks and guards pass; actual 68000 inflate-plus-Fibonacci
execution matches independently decoded bytes for all three. Overlap offsets
were remeasured for the new hashes. The native builder requires a pre-encoding
gain manifest and checks asset hashes.

Copperline PAL A500 Courtesy start/retry passed with finite nonzero captured
audio and one load notification. The rebuilt stage is 20,003 bytes and the ADF
has 75 free sectors (previously 43). Evidence: `TRACKLOADER_PREBOOST_RESULTS.json`.
Listening acceptance of the new lossy encodes remains for audition. Other
profiles remain blocked as before. The standalone load diagnostic now reads
the fresh native ADF by default, avoiding the obsolete filesystem's unboosted
assets; `--disk` selects an explicit current fixture.


## Accepted preboost audio; remaining track/cue audit

The user accepted the new encodes. Added a reproducible PC audio/binary audit
and extracted `assets/music2.cues` (8,830 bytes, PC 0.6 scaling/truncation) and
`assets/music3.cues` (9,734 bytes), retaining byte-identical music1 cues.
Five waveform windows confirm Focus is track 3, with a stable -48.5 ms offset;
Courtesy remains track 1 with -51 ms. The PC base-stage selector therefore maps
Hexagonest and its Hyper variant to Focus.

Otis/track-2 alignment is unresolved: source and PC durations differ by about
10.56 seconds, raw-waveform correlations are weak, and amplitude envelopes match
repeated passages at inconsistent offsets. A simple speed-scan did not establish
an alignment. Do not attach music2.cues to the accepted Otis bank using an assumed
constant offset. Evidence: `soundtrack/pc_track_verification.json`; reproduction:
`venv/bin/python tools/audio/verify_pc_tracks.py`.

This turn changes verification data/cue assets, not the running native ADF.
Next is loading the verified Focus cues into reusable resident storage and
extending selection; Otis needs timeline/edit mapping before cue integration.


## Otis original versus PC recording

The full-sequence comparison supersedes the earlier ambiguous local matches.
Original MP3: 156.4444 seconds; installed PC music2.dat: 145.8826 seconds, about
10.56 seconds shorter. Monotonic log-spectral alignment matches all 1,458 PC
frames at a constant +6.7-second source offset at 100 ms resolution. This supports
an approximately 6.7-second opening cut and 3.9-second ending cut, rather than
internal rearrangement or a substantial playback-speed change. Repeated phrases
had misled the independent short-window matches.

Envelope refinement estimates offsets from 6.695 to 6.646 seconds across the
recording. These are approximate, not a sample-exact cue transform. Direct
waveform agreement remains weak even after a fine speed/offset search; this
comparison cannot establish identical mastering/encoding or explain the residual
waveform difference. Use the PC recording for exact PC cue alignment rather than
assuming that trimming the accepted source will produce identical PCM.

Reproduce: `OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/compare_otis_pc.py`.
Evidence: `soundtrack/otis_pc_comparison.json`. Listening comparisons:
`scratchpad/audio/otis_pc_comparison/index.html` (8 kHz mono, timing/structure
previews, not full-band fidelity). The accepted game assets and native ADF were
not changed by this comparison.


## PC recording selected as the Otis source

At the user's request, Otis now uses the installed PC `music2.dat` recording.
`tools/audio/prepare_pc_otis.py` verifies the PC executable and recording hashes,
decodes float PCM without manual trim/shift/speed changes, and records the source
in `soundtrack/otis.pc_source.json`. `audio_sources.py` routes dictionary building
and audition encoding to the separate `scratchpad/audio/otis_pc` working tree.
The original MP3 and previous accepted auditions are preserved.

The rebuilt 12 kHz song is 1,750,594 samples (145.8828 seconds). Its boosted-before-
Fibonacci AUD package is 251,007 bytes, within 250 KiB; its zultra payload is
228,639 bytes. The expanded metadata/bank arena is 493,846 bytes. Courtesy and
Focus AUD packages are byte-identical to the accepted versions. The new audition
page is `scratchpad/audio/codec_audition_budget_250_preboost_pc_otis/index.html`;
Otis controls on this page share the PC edit, rather than synchronizing players
with mismatched recordings.

All tune validation/loop/seek/guard tests and actual 68000 combined inflate/decode
checks pass, with refreshed overlap proofs. The native ADF was rebuilt and has
69 free sectors. A standalone Copperline PAL A500 Otis IRQ diagnostic passed two
playback runs with zero underruns and at least 65 completed blocks each.
Evidence: `docs/TRACKLOADER_PC_OTIS_RESULTS.json`.

This selects and packages the PC source; it does not yet enable Otis in native
level selection. Its PCM timeline needs zero cue offset. The current player
still applies Courtesy's 612-sample correction, so a per-track cue offset must
be added before attaching music2.cues. The dictionary's approximate beat-grid
phase only selects compression slices and does not shift playback time.

Rebuild in order:

```
python3 tools/audio/prepare_pc_otis.py
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/compress_beat_bank.py --optimal --track otis --sample-rate 12000 --budget-kib 250
OPENBLAS_NUM_THREADS=1 venv/bin/python tools/audio/audition_codecs.py --budget-kib 250 --codec fibonacci --previous-budget-kib 250 --max-package-kib 250
venv/bin/python tools/trackloader/measure_payloads.py --execram-source /path/to/execram
python3 tools/trackloader/check_tune.py
python3 tools/trackloader/run_inflate_probe.py
python3 tools/trackloader/check_combined_loading.py
python3 tools/trackloader/build_native_boot.py
```


## Per-track cue timing ready for binding

The IRQ player now accepts `fib_stream_set_cue_lead(samples)` after binding and
before playback. Courtesy remains the default 612 samples; Focus uses the
verified 582-sample lead; PC Otis uses zero. The active player snapshots the
selected lead at startup. Changing it while DMA buffers are owned/running is
rejected; rebinding resets the default so timing cannot leak between tracks.
The existing Courtesy helper remains available for unchanged callers.

`make test-cue-offset` covers lead-in and cue-tick boundaries, all three leads,
integer limits and equivalence to the old Courtesy calculation. Binding tests
cover stopped/active changes, reset on rebind and rejection after unbind in both
player modes. Lifecycle/seek tests and the external-player 68000 dependency audit
pass. This is a source-level prerequisite: the native ADF was not regenerated
this turn, and Focus/Otis are still blocked in game selection. Next is resident
cue-buffer reuse and multi-track loading, with explicit cue lead selection.

## All three soundtracks connected to native level selection

The resident loader now selects Courtesy for Hexagon, PC Otis for Hexagoner,
and Focus for Hexagonest. Hyper profiles share their base profile's track;
unlock rules remain intact. Each descriptor supplies its payload, compressed
PC cue table and sample lead (612, 0 and 582 respectively). Successful bindings
are cached, so retrying the same track avoids disk loading. A failed replacement
invalidates the cache before another attempt.

The three cue tables are compressed inside the bootstrap and inflate into a
separate 16 KiB Chip buffer. Within the existing 360,448-byte Chip allocation,
the executable arena remains 196,608 bytes; the display heap is now 81,920 bytes.
The bootstrap occupies 22,073 of 22,528 reserved bytes, leaving only 455 bytes
for further bootstrap code/data. The ADF retains 69 free sectors.

A 300-second Copperline PAL A500 OCS run (512 KiB Chip + 512 KiB slow) loaded
Courtesy → Otis → Focus → Courtesy through normal level selection. A Focus retry
produced audio without another disk load. The capture contains nonzero audio
after each load and retry; the final screenshot shows the returned Hexagon run.
The write-protected disk hash was unchanged. Evidence and capture paths are in
`docs/TRACKLOADER_NATIVE_ALL_TRACKS_RESULTS.json`. Cue-offset, binding,
lifecycle, loading-barrier and Chip-arena tests pass, as do the external-player
dependency audit and host/68000 executable relocation checks.

This supersedes the earlier Courtesy-only and cue-prerequisite notes above.
Hyper selection, complete songs, NTSC and gameplay underrun counts have not been
validated by this run. Loads still block without a loading/error UI. Next work
is bounded disk-error handling and load feedback, followed by native persistence.

## Recoverable soundtrack-load error feedback

Failed preparation now leaves a persistent `LOAD ERROR` HUD banner on the
selection/results screen. Release and press Start to retry; changing selection,
returning to the menu, or successfully preparing a run clears the error. The
existing retry latch prevents held confirmation from repeatedly loading. The
resident adapter also emits `NATIVE-TUNE-FAIL` for returned disk, payload or
binding failures. No changes were made to the byte-identical historical drivers.

`tools/trackloader/make_load_failure_fixture.py` creates a separate disposable
ADF with one Courtesy payload byte changed and its OFS block checksum repaired.
This specifically exercises the resident packed-payload check before inflate.
The ordinary native ADF remains intact. Host tests cover the retained failure
state and successful-retry clearing; the new executable passes independent
host links and actual 68000 relocation checks at two addresses. The bootstrap
is now 22,099 bytes, leaving 429 reserved bytes, with 69 free disk sectors.

This is error feedback after the disk operation returns. During loading the
screen still blanks; a loading display and bounded recovery for all no-media,
wrong-disk and malformed-filesystem cases remain outstanding. The historical
DMA timeout alone is not evidence that every filesystem traversal is bounded.

Copperline PAL A500 verification passed: the damaged Courtesy payload showed
`LOAD ERROR`, a fresh Start retried and failed cleanly, and selecting Otis then
started gameplay. Screenshots and exact serial sequences are recorded in
`docs/TRACKLOADER_LOAD_RECOVERY_RESULTS.json`. The normal ADF hash was unchanged.

## Visible soundtrack loading screen

Native run preparation now switches to a static `LOAD TRACK` display after
retiring pending display publication and blits. A dedicated Copper list reloads
its eight prebuilt banner sprites every frame while the resident loader masks
CPU interrupts. Display DMA stays enabled alongside disk DMA; music/SFX remain
stopped. On either success or failure, the wrapper restores the normal COP1
dispatcher before returning to gameplay. COP2 and its frame buffers remain owned
by the normal renderer.

The loading list reuses the current render plane with both playfield colours
black, resetting its pointer each frame. A zero-bitplane sprite-only experiment
was blank in Copperline; the one-bitplane version displays the banner. No new
full-screen buffer is allocated. The native-only list occupies 256 B of BSS,
with eight small sprite allocations. Initial boot remains blank; this display
covers soundtrack preparation once the game has initialized.

The target relocation probe also now checks the EXE1 entry offset rather than
assuming entry equals the image base. The new preparation wrapper precedes the
entry point in the linked image, exposing that test assumption. Host comparison
against two independent links and actual 68000 relocation now both pass with a
nonzero entry offset.

This does not establish bounded no-media/wrong-disk recovery; those checks and
native persistence remain outstanding. DiskIO/DosIO disassembly bytes are
unchanged. The bootstrap is still 22,099 bytes, with 69 free disk sectors.

PAL A500 emulator screenshots verify the loading banner during a read, normal
gameplay after Courtesy loads, and LOAD ERROR after the damaged payload returns.
Evidence: `docs/TRACKLOADER_LOADING_DISPLAY_RESULTS.json`. Loading-barrier and
Chip-arena host tests also pass; the normal disk hash remains unchanged.

## Disk failure paths and wrong-disk recovery checked

A Copperline PAL A500 run swapped the game disk for a non-DOS ADF at 50 seconds,
then pressed Start at 60 seconds. The loader returned to LOAD ERROR. A second run
reinserted the game disk at 85 seconds and retried at 90; Courtesy loaded and the
130-second screenshot showed gameplay. Evidence and disk hash are recorded in
`docs/TRACKLOADER_WRONG_DISK_RESULTS.json`.

`tools/trackloader/check_disk_failures.py --execram-source /path/to/execram`
executes the unchanged DosIO/DiskIO instructions on Musashi with controlled
CIA/custom register inputs. No disk returns 29; DMA that never completes returns
-1; a track-zero signal that never asserts returns 30. All three preserve the
64 KiB destination sentinel and deselect the drive on return. The test includes
DosIO's cleanup, rather than calling only the low-level routine. Results:
`docs/TRACKLOADER_DISK_FAILURE_RESULTS.json`.

These are distinct levels of evidence: the disk swap was exercised in the full
emulator, while empty-drive, stalled-DMA and seek failures used mocked hardware.
The mock expires timers immediately; its cycle counts are not timeout durations.
The installed emulator CLI exposes scheduled insertion but no scheduled ejection,
so a full-emulator empty-drive/reinsert check remains outstanding.

The audit found an important remaining boundary: DosIO has no destination-capacity
argument. Its load path takes the file length from the header and copies data
before the resident caller checks the returned length. Thus the existing size
and packed-sum checks do not protect against an oversized same-named file, bad
block lengths or cyclic filesystem metadata. Before production disk swapping or
saving, replace the native read path with a bounded OFS reader (or add a verified
preflight and bounded transfer adapter); preserve the historical byte-identical
sources. Validate sector ranges, block checksums/types, file size, chain traversal
limits and destination capacity before each copy. This is now the next storage
hardening step, ahead of native persistence.

## Bounded OFS reads integrated into native boot and track loading

`trackloader/ofs.c` now replaces native DosIO file reads for both the executable
and soundtrack payloads. It accepts a trusted expected length and destination
capacity, validates the header length before copying, and checks block checksum,
type, owner, sequence, byte length and chain termination before each copy.
A visited-sector bitmap rejects repeated metadata/data blocks and bounds total
traversal. Reads outside sectors 2..1660 are rejected, protecting reserved tracks.
The supported filesystem subset is the flat ASCII-named OFS payload layout built
by this project; it does not implement subdirectories or filesystem writes.

Failures may leave a validated prefix in the destination. The native caller
never inflates, binds or publishes a failed read. Size mismatch is rejected before
any payload copies. Low-level errors propagate as failure, with motor-off cleanup.
The original annotated DiskIO/DosIO/DosIOR sources remain byte-identical.

The adapter uses unmodified DiskIO to cache full 11-sector tracks. The initial
single-sector prototype was too slow (no completed Courtesy load by the
260-second capture); it was replaced before final validation. The cache is
invalidated per file load and on read failure, and only marked valid after a
complete successful track read. Memory within the existing stage allocation:
DiskIO workspace +24000..37056, cache +38000..43632, OFS scratch +54000..55244.
The scratch is reused as tune metadata after loading. No extra allocation is
needed. The stage is 21,851 bytes, leaving 677 reserved bytes; disk free space
remains 69 sectors.

`python3 tools/trackloader/check_ofs.py` compares all six stored files byte-for-byte
and rejects 23 malformed controls, including oversized headers, block-length
violations, cycles, bad ownership, checksums and injected I/O failure. Destination
and scratch guards remain intact. The native PAL A500 emulator boots through the
new reader, loads Courtesy and displays gameplay. A separate fixture advertising
a 1 MiB Courtesy file returns to LOAD ERROR. Regenerate that fixture with
`python3 tools/trackloader/make_load_failure_fixture.py --oversize`.
Evidence: `TRACKLOADER_OFS_RESULTS.json` and `TRACKLOADER_OFS_NATIVE_RESULTS.json`.

Next validation is three-track switching and reinsertion with this reader, plus
mid-read media removal. Native persistence and its crash-safe write protocol
remain separate unfinished work.

## Bounded-reader switching and media-swap validation

Three Copperline PAL A500 runs now validate the bounded reader in the complete
native game (512 KiB Chip + 512 KiB slow):

- Courtesy → PC Otis → Focus → Courtesy loads all four requested banks. A Focus
  retry produces no extra load event, exercising the same-track reuse path.
- After Courtesy loads, replacing the disk with a non-DOS ADF makes the Otis
  request fail. Reinserting the game disk and pressing Start loads Otis.
- Replacing the disk five seconds into the first Courtesy request fails cleanly.
  Reinserting the game disk and retrying loads Courtesy successfully.

All three final screenshots were visually checked and show the resumed game.
Serial logs match the expected load/failure sequences, and the normal disk hash
is unchanged. These runs exercise per-load cache invalidation and recovery after
an interrupted read; they do not demonstrate instantaneous media-change detection
within an already cached track. No invalid/partial tune was reported ready.

Reproduce with `python3 tools/trackloader/check_native_switching.py --rom /path/to/kick13.rom`.
The script creates only the disposable non-DOS fixture, uses the write-protected
native configuration, checks exact event sequences and disk hash, and captures
screenshots for separate visual review. `--verify-existing` verifies saved logs
without launching the emulator; it assumes those logs belong to the current
image. Evidence: `docs/TRACKLOADER_OFS_SWITCHING_RESULTS.json`.

This closes the pending normal-profile switching and reinsertion checks for the
bounded reader. Empty-drive ejection, physical hardware, NTSC and audio underrun
measurements remain unverified. Next implementation work is the versioned,
checksummed two-slot save format and its recovery tests, before connecting writes
to DiskIO. Disk identity must be verified before enabling native saves.

## Save codec and two-slot recovery ready for integration

`trackloader/save.c` implements the versioned 512-byte big-endian save format,
CRC, trusted identity matching and wrap-safe selection between two generations.
It carries six best times, completion flags and opaque achievement bits without
adding achievement triggers. Invalid/unsupported slots leave outputs untouched;
conflicting equal generations or half-range ambiguity are reported separately.
Details and integration requirements are in `TRACKLOADER_SAVE_FORMAT.md`.

Host tests pass all 512 single-byte corruptions and 513 torn-write prefix lengths,
plus wraparound, identity and ambiguous-state cases. Independent Python encoding
matches the complete bytes and CRC, and rejects six valid-CRC malformed records.
The codec also compiles cleanly for 68000. No save writes are enabled and the ADF
is unchanged. Next is game-state export/import and dirty-state handling, followed
by verified disk identity, inactive-slot writes/readback and cold-boot recovery.

## Game records connected to save-state API

Game initialization now resets persistence state explicitly. A validated save
can be restored before the first simulation tick; its completion flags drive
the existing unlock rules and its best score refreshes the HUD. Improved scores
and completion-only changes mark the state dirty. Menu/results snapshots coalesce
updates; only acknowledgement after verified storage commit can clear dirty state.
Acknowledging an older snapshot preserves any newer RAM improvements, while
advancing the last committed generation correctly across wraparound.

`make test-game-save test-track-save test-game-loading` passes, including restore
and unlock behaviour, disallowed gameplay/death snapshots, failure retention,
stale acknowledgements and completion-only updates. The native sources link
without OS imports. This step adds the gameplay boundary, not disk persistence;
the previous tested ADF remains unchanged and must be rebuilt after integration.
The next work is trusted media/slot identity and write/readback plumbing, then
cold-boot save recovery. API details: `TRACKLOADER_SAVE_FORMAT.md`.

## Save transaction and trusted media manifest prepared

The new RAM-side transaction layer verifies trusted immutable sector snapshots,
selects the inactive raw slot, checks generation, writes it, then verifies full
readback and identity before reporting success. The slots are sectors 1738 and
1749 on separately reserved physical tracks. Identity mismatches, conflicting
slots, failed writes and failed verification do not acknowledge game state.

`make test-save-disk test-game-save test-track-save` passes. The transaction code
compiles cleanly for 68000. `make_save_manifest.py` independently verifies the
current built image and emits trusted boot/root/bitmap snapshots plus a
build-specific save identity. The image itself is unchanged.

This remains adapter-level work: native DiskIO writes, media-change handling,
manifest embedding, startup restore, conflict/retry reconciliation and cold-boot
persistence are not connected yet. In particular a failed readback after a real
write may have created a newer valid slot; integration must reconcile that case
without losing dirty RAM records. Full contracts are in `TRACKLOADER_SAVE_FORMAT.md`.

## Startup recovery and uncertain-write retries validated

The storage API now loads and validates both raw slots with identity checks before
and after reading. Blank factory slots return explicit defaults; damaged/conflicting
media or I/O errors preserve the caller's state. An exact snapshot already committed
can be retried successfully without another write, resolving a completed write
whose previous readback failed. Different contents at the same generation remain
a conflict. Integration must retain the pending snapshot for this retry path.

Host tests cover load failures, blank defaults, damaged-newest fallback and exact
retry. The actual C transaction also passes a disposable-ADF test: two generations
are written, the image is reopened with fresh state, latest records recover, and
corrupting the newest slot recovers the previous state. All other sectors and the
source disk are unchanged. Report: `TRACKLOADER_SAVE_IMAGE_RESULTS.json`.

The updated code compiles for 68000. Native startup calls, manifest embedding,
DiskIO write callbacks and UI are still pending, so this is not a claim of game
cold-boot persistence. Next is the resident adapter and boot/main-loop wiring.

## Native cold-boot save reads connected

The resident now reads the two raw save slots with DiskIO and validates/selects
them through the relocated save codec. `TrackGameBoot` carries the decoded startup
state to `game_restore_save` before simulation starts. The trusted payload identity
is embedded; write-authorization anchor metadata is generated separately. The
stage is 22,055 bytes (473 bytes spare); the disk now has 67 free sectors.

PAL A500 emulator screenshots show 75.01 seconds from the newest valid generation,
65.01 from the older slot when the newest is damaged, and zero defaults for foreign
identity. All tests use write-protected seeded images. Host save/game/OFS tests,
independent relocation checks and actual 68000 relocation pass; the historical
assembly still compiles byte-identically. No native writes have been enabled.

Next remains resident write/readback integration, media-change protection, save
status UI and an end-to-end game-written save/reboot test. Defaults on a rejected
startup save must not authorize overwriting ambiguous or foreign slot contents.

The restored completion flag also exposes HYPER HEXAGON through normal menu
selection. Evidence: `TRACKLOADER_SAVE_STARTUP_RESULTS.json`.

## Native saving and cold reboot verified

Native save writes are now connected end to end. The game saves coalesced records
at menu/results boundaries, displays SAVE during I/O, and acknowledges only after
identity checks and full readback. Failures display SAVE ERROR and retain the
pending snapshot for a later run's retry. The resident write adapter restricts
writes to the two raw slots, uses uncached reads and checks the drive-change latch
between transfers. Trusted anchor snapshots are embedded compressed and expanded
into the unused stage region at +45000.

A PAL A500 Copperline run wrote a 6.90-second Hexagon record to slot A. After
reboot, a Hexagoner run wrote 2.31 seconds to slot B, preserving the earlier record.
A final cold boot displayed the game-written Hexagoner best in the menu. Both
save CRCs and identities validate; only the two reserved sectors changed. A
write-protected run displayed SAVE ERROR without changing its disk. Evidence:
`TRACKLOADER_NATIVE_SAVE_RESULTS.json`. Original assembly byte-identity, host
save/game tests and independent/68000 relocation checks pass.

The current stage is 22,517 bytes, only 11 bytes below its reserved limit. The
game image uses 196,504 of 196,608 bytes, leaving 104 bytes. There are 64 free disk
sectors. Build assertions enforce these limits. Reclaiming resident/image space
is the next implementation priority before adding features. Then validate save
interruption/removal, physical hardware and NTSC. Achievement triggers and
cross-build migration remain outstanding; this is not completion of every plan
exit gate. This section supersedes earlier statements that native writes are
not connected.

## Resident and executable space reclaimed

Save modules now compile for size, as do the boot executable validator and
resident glue. Frame rendering, audio IRQ code, tune expansion and decode keep
their existing optimization settings. The 2 KiB save transaction workspace moved
from executable BSS to unused resident Chip RAM at +46800..48848, with an explicit
boot pointer, compile-time size check and build-time non-overlap checks.

The stage decreased from 22,517 to 22,169 bytes: spare capacity increased from
11 to 359 bytes. Executable resident memory decreased from 196,504 to 194,036:
spare capacity increased from 104 to 2,572 bytes. Total Chip allocation and heap
size are unchanged; disk free space remains 64 sectors. No gameplay or save-format
rules changed. Loading performance has not been benchmarked against the prior
build; no FPS claim is made.

Host save/OFS tests and independent/actual-68000 relocation validation pass.
An emulator run loads Courtesy, saves, switches to Otis and saves again using
the relocated workspace. Both slots have valid checksums; only sectors 1738 and
1749 change. The prior save trial is preserved separately as `save_write.adf`;
this build uses `space_save.adf`. Build-specific identity still means old-build
saves are not migrated automatically.

The cold reboot restores the game-written Hexagoner best (3.01 seconds) to the
menu. Detailed before/after sizes and evidence: `TRACKLOADER_SPACE_RESULTS.json`.

## Interrupted-save recovery strengthened

The new transaction suite passes 2052 prefix/transport fault cases across both
slot orientations. It found a recovery gap: startup previously abandoned both
slots if either disk read failed. Startup and the C storage loader now discard
partial failed-read output and recover independently from the other slot. Commit
remains conservative and refuses writes if either slot cannot be read.

Two extended-ADF emulator tests replace one complete save track with raw data
having no valid sector headers. DiskIO reports failure, yet startup restores the
intact slot and displays the expected best time (75.01 with A unreadable, 65.01
with B unreadable). Both disks remain unchanged. This validates damaged-track
recovery in PAL A500 emulation; it is not a timed power-cut or real-drive test.
Evidence and reproducible fixture scripts: `TRACKLOADER_SAVE_INTERRUPTION_RESULTS.json`.

The stage is now 22,209 bytes (319 spare); game memory remains 194,036 bytes
(2,572 spare). Historical assembly byte-identity and relocation checks still pass.
Next validation remains write-time media removal/power interruption and target
hardware/NTSC behavior; achievement triggers and cross-build migration are also
still outstanding.

## Save media-change transaction boundaries

`make test-save-media-change` now exercises 84 media-change faults: all 13
commit transfer boundaries and all eight startup-load boundaries, with either
slot active and both foreign and identity-identical replacement disks. The
callbacks model an adapter that latches a media change and rejects subsequent
transfers. Failed reads deliberately still copy replacement bytes to scratch.

Every commit fault returns an error, preserves the active slot and leaves the
replacement unchanged. Reinserting the original and retrying the same snapshot
succeeds; if the first attempt had already written the complete snapshot, retry
acknowledges it without another write or generation increment. Every startup
fault preserves the caller's output. The existing 2052 interruption cases and
storage/game-save tests also pass.

This establishes the C transaction's behavior under the adapter contract, not
physical media-change safety. The native adapter checks the CIA disk-change
latch between calls. Removal/replacement inside a DiskIO seek or write remains
unvalidated, as does retention of the change indication across driver stepping.
Timed emulator swaps and real-drive tests remain open. No runtime or disk asset
changes were needed for this test addition; native size figures above still apply.

## Seek-time media-change gap reproduced

`tools/trackloader/check_disk_change.py --execram-source /path/to/execram`
executes the unchanged DiskIO binary on a 68000 with mocked CIA inputs. A
replacement is modeled after the wrapper's check but before the driver's seek.
When a step clears the modeled change latch, execution matches the no-change
control: six read-DMA starts followed by the expected timeout (no MFM input is
provided). A permanently asserted latch instead returns error 29 with no DMA.
See `TRACKLOADER_DISK_CHANGE_RESULTS.json`.

The driver seeks at +$0106 before checking the latch at +$0110. Its write
routine checks write protection at +$0208 but has no equivalent change check
before arming write DMA at +$024a/$0250. Consequently, boundary checks and
post-write identity verification alone cannot establish that a replacement
medium is never written. The latter can detect a mismatch only after damage.
This probe demonstrates lost change detection, not an actual wrong-disk write;
physical timing and valid replacement MFM remain untested.

This is an open safety gate for native saving. Further work needs a separately
maintained native driver adaptation that detects/latches changes during seeking
and before write DMA, plus injection during real track reads/writes. Checks must
cover retry/recalibration paths as well as the main path. A software check alone
cannot make physical removal during an already active write atomic. Preserve
the original three disassemblies byte-identically; their verification still
passes. No production code or ADF was changed by this diagnostic addition.

## Native save driver guards

The native build now derives `guarded_diskio.s` from the archival `DiskIO.s`
using `tools/trackloader/make_guarded_diskio.py`. It adds checks before each seek
step (including recalibration/retry steps), at write entry, and immediately
before write DMA setup. A detected change exits the whole driver call through
normal cleanup with error 29; nested helper return addresses are discarded via
the driver's frame. The adapter enables guards only after the first identity
read in a save sequence and disables them after each call, so ordinary boot and
soundtrack reads retain the original insertion/recovery behavior.

The actual-68000 probe now compares original and guarded binaries. In the
seek-cleared change model, the guarded version returns 29 with zero steps and
zero DMA starts; the no-change timeout control still runs and cleanup deselects
the drive. The original disassembly byte checks and 84 boundary/2052 interruption
cases continue to pass. The stage grows to 22,269 bytes, leaving 259 bytes.

This narrows the known race but does not make physical writes atomic. Changes
between a guard and its following hardware operation, removal during active DMA,
and write-path MFM fault injection remain validation gaps. The probe directly
exercises the seek guard; it does not yet establish write-path fault behavior.

A disposable writable Copperline PAL A500 trial boots this image, loads Courtesy
and saves a 359-tick (5.98-second) result. Only sector 1738 changes; generation 1,
identity and CRC validate. Screenshot review shows the expected 005.98 result.
Evidence: `TRACKLOADER_GUARDED_SAVE_RESULTS.json`. This is normal-path validation,
not timed-removal validation.

## Write-path guard execution tests

`tools/trackloader/check_disk_write_guards.py --execram-source /path/to/execram`
now executes the actual guarded 68000 write/retry routine. The harness bypasses
only the preceding MFM read with a successful return, preserving the real driver
stack frame. Symbol offsets are exported by the assembled harness, rather than
assuming instruction offsets. CIA timers expire immediately and DMA times out.

Eight trials cover no change, change at write entry, change immediately before
DMA setup, and change at the first retry, with guards both enabled and disabled.
With guards enabled, either pre-write fault returns 29 with no DMA-arm writes.
A retry fault returns 29 after the first attempt's two DSKLEN writes and prevents
further attempts. Disabled controls issue six arm writes (three attempts), as
does the guarded no-change control. Every trial restores SP, leaves the caller's
buffer intact and deselects the drive. Evidence:
`TRACKLOADER_WRITE_GUARD_RESULTS.json`.

No production changes were needed. These tests validate write-path control flow
and error unwinding, not valid MFM transfers, completed writes or removal while
physical DMA is active. Those timing/media validation gates remain open.

## Active-DMA change handling

The write harness now has 14 trials, adding changes after DMA is armed with
both timeout and completion signals, plus a normal-completion control. Without
checking the polling loop, the driver can return success when change and DMA
completion arrive together. The native derivative now checks the change latch
before accepting completion in `diskio_L05ec`. Its abort path explicitly disables
DSKLEN and clears the disk-block interrupt before unwinding and motor cleanup.

All 14 trials pass. Active-DMA changes return error 29 without a second attempt;
normal modeled completion returns zero. Every trial verifies DMA disabled on
return, restored stack, unchanged caller buffer and deselected drive. Seek tests
and original byte-identity checks still pass. The rebuilt stage is 22,289 bytes
(239 spare). This latest image has not yet had a full-emulator boot/save trial;
the earlier guarded-save report belongs to the preceding build.

These are actual-68000 control-flow tests with mocked hardware signals and a
stubbed preceding MFM read. They cannot prove what bytes reach a disk during
removal: stopping DMA cannot undo bytes already written. Timed full-emulator
swaps and physical drive validation remain outstanding. Polling adds work during
synchronous save transfers; no gameplay FPS or real-time disk latency claim is
made. The first identity read still deliberately uses the unguarded insertion
path; subsequent transfers in the save sequence enable the guards.

## Latest DMA-guard build: full-emulator save and cold reboot

The 22,289-byte stage now passes a normal-path Copperline PAL A500OCS trial
(512K Chip + 512K slow). It boots, loads Courtesy and writes a 359-tick result
(5.98 seconds). Only reserved sector 1738 changes, with generation 1 and valid
identity/CRC. A separate cold boot from a read-only copy reports
`NATIVE-SAVE-RESTORED` and displays 005.98 in the Hexagon menu; the reboot disk
is byte-identical to the saved disk. Both screenshots were visually checked.

`tools/trackloader/check_dma_guard_save.py` verifies source-build hash, changed
sectors, save contents, serial events and unchanged reboot media. Evidence is in
`TRACKLOADER_DMA_GUARD_SAVE_RESULTS.json`; disposable artifacts use the
`dma_guard_save` and `dma_guard_reboot` prefixes. This closes the latest build's
normal boot/save/restore validation gap. Timed swaps during an actual write,
physical-drive behavior and NTSC validation remain open.

## Timed full-emulator swap during the save window

A PAL A500OCS control run (Space at 60 seconds) displays SAVE at 114 seconds.
A separate fresh-disk run inserts a foreign blank image at 115 seconds and
shows SAVE ERROR at 140 seconds. Both the original image and replacement remain
byte-identical to their inputs. The game returns to its rendered error screen
rather than remaining in the synchronous save display. Screenshots were reviewed.
Evidence: `TRACKLOADER_SAVE_SWAP_RESULTS.json`; artifact verifier:
`tools/trackloader/check_save_swap.py`.

Copperline's `--insert-disk-after` explicitly sets the replacement's
`write_protected` flag to true (`src/main.rs`). Therefore unchanged replacement
bytes do not establish writable-media protection. The SAVE display covers the
whole transaction, including identity reads, so these captures also do not
establish a swap during active write DMA. This closes a timed full-emulator
save-failure/UI check, while precise transfer-phase swaps, writable replacements,
reinsertion/retry and physical-drive tests remain open. Disposable fixtures use
`save_window`, `save_swap` and `save_swap_foreign` prefixes. No production code
changes were required.

## Pending-save retry orchestration

`make test-native-save-retry` now exercises the actual native save controller
with hardware operations and transaction results stubbed. Two failed attempts
retain the original snapshot without acknowledgement or repeated menu-frame I/O.
A later successful retry acknowledges that snapshot, preserves newer RAM progress,
and commits the newer result under the next generation on the following tick.
The test also checks that entering play clears the error and enables a retry.
This complements the real storage transaction and game-boundary tests; it does
not emulate hardware or verify disk bytes.

A full-emulator trial (`save_reinsert`) swaps to foreign media at 115 seconds,
reinserts the original at 145 and presses Space at 150, capturing at 225. The
final screen shows SAVE ERROR and the original remains byte-identical. Scheduled
reinsertion makes it read-only. No transfer instrumentation or intermediate
capture establishes the second attempt in this trial; do not treat this final
screen alone as proof of retry execution or successful pending-save persistence.
Writable reinsertion/retry remains a full-emulator validation gap. No production
changes were made for this test addition.

## Native retry tested with real storage transactions

`make test-native-save-retry` now links the real `save.c` and `save_disk.c`
instead of stubbing transaction results. The actual native controller runs
against memory-backed sector I/O; hardware operations and the gameplay API remain
stubbed. The trial covers two write-protected failures, writable reinsertion,
and a completed write whose readback fails. The next attempt acknowledges that
existing generation without rewriting it; newer RAM progress then writes the
alternate slot as generation 2. Both slot payloads decode with the real codec,
and the real selector restores the newer result. Exactly two writes occur over
five transaction attempts. The pending snapshot survives failures without an
acknowledgement or a repeated menu-frame retry.

This supersedes the earlier stubbed-transaction retry coverage. Game-boundary
and storage transaction tests also pass. It establishes the integrated controller/
codec/transaction behavior in a host harness, not full-emulator writable
reinsertion or physical disk behavior. No production code changes were needed.

## Writable reinsertion and retry verified in the installed emulator

Copperline 0.20.0's control protocol supports `media.floppy.insert` with explicit
`write_protected: false`, despite the scheduled CLI option forcing read-only
media. `tools/trackloader/check_writable_retry.py` now uses the installed,
unmodified emulator and this interface to run a reproducible disposable trial.
No diagnostic emulator modification is required.

The run pauses at 115 seconds on SAVE, inserts a writable foreign image, and
shows SAVE ERROR at 140. At 145 it reinserts the writable original; Space at 150
starts another run, visibly playing at 153. At 225 the result screen has no save
error. The original now contains a valid generation-1 save of the retained
359-tick (5.98-second) result, with matching identity and CRC. Only sector 1738
changed. The writable foreign image remains unchanged. All four screenshots
were visually reviewed; control calls and responses are retained in events.json.
Evidence: `TRACKLOADER_WRITABLE_RETRY_RESULTS.json`.

This closes the full-emulator writable-reinsertion/retry gap for this scenario.
The exact transfer phase during the 115-second swap remains uninstrumented: a
SAVE screen includes identity reads as well as writes, so this is not yet an
active-write-DMA removal test. Physical-drive validation remains outstanding.
An initial diagnostic build from an older local Copperline checkout encountered
read failures and was excluded; only the installed 0.20.0 control-protocol run
supports these results.

## Swap immediately after write DMA activation

`tools/trackloader/check_active_write_swap.py` now uses a persistent control
connection and a DSKLEN register watchpoint in the installed Copperline 0.20.0.
The connection must persist because debugger points are removed on disconnect.
After the transaction's read transfers, the probe records two consecutive CPU
writes of DSKLEN=$D961 at CCK 414055922 and 414055930. It inserts writable foreign
media immediately after the second arm write, at 116.737577515 emulated seconds.

Fifteen emulated seconds later the game displays SAVE ERROR. Both writable ADFs
remain byte-identical to their inputs; neither original nor replacement has a
changed sector. The final screenshot was reviewed. Exact stop events, control
calls and captures are retained under the report's evidence directory:
`TRACKLOADER_ACTIVE_WRITE_SWAP_RESULTS.json`.

This closes the previously uninstrumented DMA-activation boundary in emulation.
It tests immediately after arming, not removal later in the track write or a
sweep of DMA phases. Copperline's ADF insertion resets its cached track and
rotation state; real-drive behavior is not established by this result. Physical
validation and later-phase removal remain outstanding. No production code
changes were required.

## Later write phases: replacement-media corruption found in emulator trial

The active-write harness now accepts `--delay-ms`. It leaves the DSKLEN watch
armed during the delay and requires reaching the requested time without another
DSKLEN write, so it does not silently test after the driver disables DMA.

At 10 ms after the second $D961 write, the game returns to SAVE ERROR and both
writable images remain unchanged. At 100 ms it also returns to SAVE ERROR, but
the replacement image acquires sector 1738 containing a complete valid HXS1
save: generation 1, the expected identity, 359 ticks and valid CRC. The original
remains unchanged. The harness correctly fails its replacement-integrity
assertion and preserves all evidence. Reports:
`TRACKLOADER_ACTIVE_WRITE_SWAP_RESULTS_10MS.json` and
`TRACKLOADER_ACTIVE_WRITE_SWAP_RESULTS_100MS.json`.

Source inspection of the checksum-verified Copperline 0.20.0 release points to
an emulator media-lifetime problem: `src/floppy/mod.rs::insert_disk_image`
replaces `drive.image` without retiring the active DMA's buffered `write_words`.
Disabling DSKLEN invokes `finish_write_dma`, and `finish_write_words` resolves
the destination through the drive's *current* image. Thus data collected before
the swap can be applied to the replacement when the game's guard stops DMA.
This is a source-supported explanation, not a patched-emulator comparison or a
physical-drive finding. The guard should continue stopping DMA; suppressing that
cleanup to accommodate an emulator artifact would be inappropriate.

The immediate-arm and 10 ms passes do not establish safety for later writes.
Direct instantaneous writable replacement currently has this known failing
case. Next useful validation is removal with a real no-media interval, and an
isolated emulator media-lifetime regression/fix if pursued. Physical-drive
validation remains open. Production game code is unchanged.

## Active-write removal with an empty-drive interval

The harness now accepts `--eject-gap-ms`. At 100 ms into the write it ejects the
original, runs with no medium for 20 ms, then inserts writable foreign media.
The persistent DSKLEN watch records the CPU writing $4000 (DMA disabled) about
1.013 ms after removal, before the replacement is inserted. The final screen
shows SAVE ERROR. Both original and replacement ADFs remain unchanged.

Evidence: `TRACKLOADER_ACTIVE_WRITE_SWAP_RESULTS_100MS_GAP20MS.json`. Reproduce
with `check_active_write_swap.py --delay-ms 100 --eject-gap-ms 20 --rom ...`.
This demonstrates the guard stopping an in-progress write during a no-media
interval in the installed PAL A500 emulator. It contrasts with the preserved
instantaneous replacement failure at the same write phase, supporting the
buffered-write/media-lifetime explanation documented above.

The measured stop latency is specific to this emulator and injection point;
it is not a physical-drive guarantee. Standard ADF persistence also does not
model all partial magnetic-track damage. Keep the instantaneous-swap failure
open; this passing scenario does not supersede it. No production changes were
needed.

## Unified prepared-workspace build entry point

`make trackloader-adf EXECRAM_SOURCE=/path/to/execram` now runs the native game
link/package/relocation/inflate/image chain and publishes a hash-named factory
ADF plus a JSON manifest. `trackloader` is an alias. Preflight checks missing
tools and prepared assets before compilation; publication refuses to overwrite
an existing image whose bytes changed. Named emulator save disks are untouched.

The full command passed and reproduced the exact previously tested image hash
`96017d688d3749bf07cda0f30160e38943500d43699c3b29eae46bcb9a0d2460`.
`make test-trackloader-storage` also passes all six component suites.
See `TRACKLOADER_BUILD.md` for usage and dependencies. Clean-checkout soundtrack
preparation, portable tool discovery and complete release validation remain
separate outstanding work; this entry point explicitly requires the prepared
workspace and its tune validation reports.

## Regenerating prepared disk payloads

`make trackloader-assets EXECRAM_SOURCE=/path/to/execram` rebuilds the Zultra
library, packs the accepted preboosted/PC-Otis AUD1 banks and regenerates the
actual-68000 inflate proofs. The pipeline passed, followed by the factory ADF
build. The image remains byte-identical to the prior tested build.

The packaging/reference decode no longer imports the audition tool's NumPy and
SciPy dependencies. A standard-library FIB1 reference decoder is checked against
the actual C expansion code across all three production banks, 1,031 synthetic
lengths and six malformed cases; all pass. Original recording analysis and
encoding remain separate and still need their audio dependencies.

The native image builder now derives the fixed stage/save reservation ranges
from its disk ABI instead of reading the historical trial-layout report. It
still checks every reserved sector is free before changing the bitmap and runs
the layout validator afterward. The release preflight no longer requires that
obsolete report. See `TRACKLOADER_BUILD.md` for the two build commands and
remaining source-asset prerequisites. This is a reproducible path from approved
AUD1 banks, not yet a clean-checkout path from original recordings.

## Remaining scratch-tool dependency removed

The release chain now builds the original DosIO filesystem writer via
`build_dosio_probe.py`; it no longer implicitly depends on executables produced
by the old filesystem trial. That trial uses the same helper, avoiding duplicate
compiler recipes. The full release build passed and again produced the exact
previously tested factory ADF hash.

Asset preparation now validates every accepted AUD1 bank before its first output
write: header, preboost/rate, Fibonacci structure, PCM offsets and sequence
bounds/sample total. `--check-only` exposes this read-only preflight; all three
production inputs pass. Missing compilers/assembler are also caught before
payload generation. These checks do not make the whole multi-step build atomic;
a later compiler or validation failure can still leave intermediate outputs.

## Accepted soundtrack identity checks

The accepted preboosted banks, compressed tunes and PC cue files now have an
explicit SHA-256/size lock in `soundtrack/trackloader_assets.lock.json`. Preparation
checks source identity before output writes and verifies generated disk assets;
the release command verifies disk assets before compilation and records the lock
hash in its manifest. These checks complement structural validation and are not
silently refreshed when an input changes.

All nine current inputs validate. Eighteen modified/missing-asset cases reject
as expected in a temporary test workspace. The full factory build still produces
SHA-256 `96017d688d3749bf07cda0f30160e38943500d43699c3b29eae46bcb9a0d2460`.
Intentional future soundtrack changes require an explicit lock update; this
neither regenerates the original recordings nor closes physical-hardware gates.

## Native PAL/NTSC detection and initial NTSC trial

The resident stage no longer supplies a hard-coded PAL boot flag. After masking
interrupts it observes the raster across line 256 and detects PAL only if the
frame reaches line 300 before wrapping. This handles either starting half of a
frame without relying on OS services. It supplies the detected flag through the
existing ABI to `video_select`, selecting the existing 50/60 Hz game timing,
visible height and Paula periods. This targets standard A500 video modes.

The same image boots, enters gameplay and loads Courtesy in Copperline PAL and
NTSC A500OCS (512K Chip + 512K slow). Space is pressed at 60 seconds; both runs
reach the rendered SAVE ERROR screen at 140 on intentionally read-only disks.
Screenshots were reviewed and both disk copies remain unchanged. These silent
runs do not establish audio quality/timing or the full NTSC lifecycle. Evidence:
`TRACKLOADER_VIDEO_BOOT_RESULTS.json`; verifier `check_video_boot.py`.

New image SHA-256: `fd37804a4aa739736305c710aefe6f049e8745f8efaac6c1e2ecd148d17b12ca`.
Stage size is 22,375 bytes (153 spare); game memory is unchanged. Earlier save
and disk-removal reports apply to their recorded older image hashes. NTSC
multi-track switching, writable save/cold restore, and physical hardware remain
open. The plan's obsolete “no runtime changes” heading has been corrected.

## NTSC writable save and cold restore

The auto-detecting image passes a writable-save trial in Copperline NTSC
A500OCS, 512K Chip + 512K slow. It boots, loads Courtesy and stores 191 ticks
(3.18 seconds) as generation 1. Only reserved sector 1738 changes; identity and
CRC validate. Cold booting a separate read-only copy reports
`NATIVE-SAVE-RESTORED` and displays 003.18 on the Hexagon menu. Both screenshots
were reviewed; the reboot image is byte-identical to the saved disk.

Evidence: `TRACKLOADER_NTSC_SAVE_RESULTS.json`; artifact verifier:
`tools/trackloader/check_ntsc_save.py`. This validates the basic NTSC save/restore
cycle on image `fd37804a4aa739736305c710aefe6f049e8745f8efaac6c1e2ecd148d17b12ca`.
NTSC all-track switching/audio timing, disk-change fault cases and physical
hardware remain unvalidated. These silent runs make no audio-quality claim.
No production changes were required.

## NTSC all-track switching and load-failure recovery

`check_native_switching.py` now accepts `--video PAL|NTSC` and an optional
`--scenario` selector. NTSC artifacts/reports use separate names, preserving the
PAL trial history. On the auto-detecting image, all three NTSC scenarios pass:
Courtesy -> Otis -> Focus -> Courtesy; wrong-disk rejection then successful Otis
load after reinsertion; and a mid-read swap followed by successful Courtesy
retry. Exact serial sequences match the expected READY/FAIL events, with no
extra failure events, and the factory ADF remains byte-identical.

The three final screenshots were reviewed. Each returns to a rendered game
screen with the expected SAVE ERROR on the intentionally read-only image.
Evidence: `TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC.json`. Reproduce all scenarios
with `check_native_switching.py --video NTSC --rom ...`, or use
`--video NTSC --verify-existing` to check captured artifacts.

These are silent loader/lifecycle tests, not audio quality, cue timing, full-song
loop or underrun measurements. NTSC write-time media-removal tests, physical
hardware, and the documented emulator instantaneous-swap issue remain open.
No production code changes were required.

## NTSC player timing fields and underrun counters

`check_ntsc_player.py` locates the relocated game through a unique relocation-free
code signature, resolves symbols from the matching ELF, and reads the native
player's counters using the installed emulator control protocol. It makes no
guest memory writes. Runtime video fields are exactly `(60,262,200,298,447)`:
frame rate, lines, visible height, music period and SFX period.

The Courtesy -> Otis -> Focus -> Courtesy sequence reports respectively 92,
146, 62 and 74 completed audio blocks, with zero underruns in each sampled run.
Counters are sampled after normal play/death; they reset on each player start.
Serial READY events confirm the expected track order. The read-only factory
image is unchanged. Evidence: `TRACKLOADER_NTSC_PLAYER_RESULTS.json`.

This supplies actual player-counter evidence in NTSC emulation, beyond the
previous silent loader tests. It is still not a waveform-quality/cue alignment
measurement, full-song loop test or physical-machine performance claim. The
short runs do not establish absence of underruns across entire levels. Game and
ADF contents are unchanged.

## Diagnostic NTSC Courtesy full-song endurance

`check_ntsc_loop.py` runs the production image with a documented emulator-RAM
patch: the first instruction of `pc_collide` is replaced with RTS so ordinary
collisions do not end the run. The original instruction and relocated address
are recorded; no source, ELF or ADF bytes are modified. Thus it exercises normal
rendering, pattern progression and audio interrupts, but is not an unmodified
playthrough or a collision-performance benchmark.

Courtesy's sequence contains 2,320,000 samples (193.33 seconds at its nominal
12 kHz rate). Between 110 and 313.333 emulated seconds, the player completes
4,727 blocks / 2,420,224 samples, exceeding the full sequence length. It remains
running at every sample and reports zero underruns throughout. Only one tune
READY event occurs; no restart accounts for the continued stream. The final
screenshot shows ongoing gameplay. The factory disk remains byte-identical.
Evidence: `TRACKLOADER_NTSC_LOOP_RESULTS.json`.

This demonstrates continuous playback across at least one sequence wrap under
NTSC emulation with collision handling bypassed. It does not establish waveform
seamlessness, cue alignment, physical-machine timing or full-song endurance for
Otis and Focus. Those remain separate validation gates.

## NTSC Otis and Focus full-song endurance

The diagnostic loop harness now selects a track with `--track courtesy|otis|focus`
using ordinary menu input before starting. Separate Otis and Focus runs retain
the collision-bypass patch solely in emulator RAM and leave their ADFs unchanged.
Both final screenshots were reviewed and show continuing gameplay.

Otis consumes 1,855,488 samples over the measured interval, exceeding its
1,750,594-sample sequence. Focus consumes 2,049,536 samples, exceeding its
1,946,667-sample sequence. Every sample reports the player running with zero
underruns, and each run contains exactly one expected tune READY event. Thus all
three accepted soundtracks now have diagnostic NTSC endurance evidence across
at least one sequence wrap. Reports: `TRACKLOADER_NTSC_LOOP_RESULTS_OTIS.json`
and `TRACKLOADER_NTSC_LOOP_RESULTS_FOCUS.json`, alongside the Courtesy report.

These remain collision-bypassed, silent emulator tests. They do not validate
waveform seams, cue alignment, physical-machine timing or the collision workload
of an unmodified long playthrough. Production source and disk bytes are unchanged.

## Cue wrap and coverage audit (before all-PC source update)

`check_cue_wrap.py` checks the actual C sample-position/index functions against
sample sequences straddling each production song boundary at both Paula periods.
It reads lead offsets from the generated native descriptors. All 27,594 cases
pass: song position wraps within a DMA block and cue indexing returns to the
start without carrying the previous loop's index.

Coverage is not identical to audio duration. Courtesy has 31,188 samples
(2.599 seconds) beyond its cue table, where the player returns cue zero. Otis's
cue table extends 15,406 samples (1.28383 seconds) beyond its encoded song, and
Focus's extends 715 samples (0.05958 seconds). These measurements include the
configured leads of 612/0/582 samples. Report: `TRACKLOADER_CUE_WRAP_RESULTS.json`.

These differences alone do not prove misalignment: cues can omit a quiet tail
or extend beyond a recording. Do not pad, trim or retime accepted audio based
only on these lengths. Waveform/PC-cue comparison around song endings remains
needed to decide whether a correction is appropriate. No production assets or
code were changed by this audit.


## All-PC soundtrack source update

At the user's request, Courtesy, Otis and Focus now all use the PC recordings
(`music1.dat`, `music2.dat`, `music3.dat`). Source hashes are checked by
`tools/audio/prepare_pc_tracks.py` and recorded in `soundtrack/*.pc_source.json`.
The new audition page is
`scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/index.html`.
All three use 12 kHz mono, boost before Fibonacci encoding, and packages below
250 KiB. Otis remains byte-identical; Courtesy and Focus are newly encoded.
Their old MP3 cue offsets have been removed from native tune descriptors.

The new factory ADF is
`out/trackloader/823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3.adf`.
Stage size is 22,373 bytes and 60 disk sectors remain free. Payload identity is
`7f0a0e0f8d1460bf8a1455ccb5347266`; prior audio-build save identities differ.

Target in-place inflate, actual C Fibonacci expansion, accepted-asset identity
and all 27,594 host cue-wrap cases pass with the new assets. Updated coverage:

| Track | Samples at 12 kHz | Cue lead | Unused cue extent |
| --- | ---: | ---: | ---: |
| Courtesy | 2,273,678 | 0 | 1.21017 s |
| Otis | 1,750,594 | 0 | 1.28383 s |
| Focus | 1,933,426 | 0 | 1.11450 s |

No song now extends beyond its cue table. These remaining cue-table tails
are measured against the untrimmed PC recordings; audio has not been padded
or stretched to match them. Full waveform/cue alignment remains unmeasured.

The new image also passes the Copperline PAL A500 switching sequence:
Courtesy → Otis → Focus → Courtesy, with all four `NATIVE-TUNE-READY` events
and unchanged disk bytes. Report:
`TRACKLOADER_OFS_SWITCHING_RESULTS_OFS_SWITCHING.json`. This was a silent
loading test, not a listening or full-song endurance test of the new encodes.

## All-PC image endurance and NTSC switching

The same `823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3`
image now passes NTSC Courtesy → Otis → Focus → Courtesy switching, as well as
individual NTSC endurance runs for all three PC tracks. Each run observes
continuous playback beyond a complete song length with zero DMA underruns:

| Track | Song samples | Samples consumed during observation | Underruns |
| --- | ---: | ---: | ---: |
| Courtesy | 2,273,678 | 2,374,144 | 0 |
| Otis | 1,750,594 | 1,855,488 | 0 |
| Focus | 1,933,426 | 2,036,736 | 0 |

`tools/trackloader/check_pc_audio_release.py` checks the current asset lock,
PC source hashes, zero native cue leads, loaded sample lengths, PAL/NTSC
switching logs and all three endurance reports against the current ADF hash.
It rejects stale evidence from earlier audio builds. Combined result:
`TRACKLOADER_PC_AUDIO_RELEASE_RESULTS.json`.

These are silent A500OCS emulator checks with 512 KiB Chip and 512 KiB slow RAM.
The endurance runs bypass collisions in disposable emulator RAM; source and
ADF bytes are unchanged. Audible seams/cue alignment and physical-machine
behaviour remain outside this validation.

## Production loop-boundary auditions

`python3 tools/audio/audition_pc_loops.py` reconstructs each song directly from
its locked Zultra/TUN1 Fibonacci payload and verifies sample-for-sample equality
with the full encoded audition. It generates eight-second excerpts: four seconds
before the wrap and four after, with no fades or crossfades. Compare the boosted
12 kHz PC references with production payloads at
`scratchpad/audio/pc_loop_audition/index.html` (wrap at 0:04).

The boundary sample jumps in signed 8-bit units are Courtesy 0, Otis 2 and
Focus 0, identical to their corresponding PC references. This checks the exact
sample discontinuity, not perceived loop quality. Host previews omit Paula
filtering; no hardware audio capture has been made. The game and disk assets
are unchanged. Results: `TRACKLOADER_LOOP_AUDITION_RESULTS.json`.

## Current all-PC image save/reboot verification

`check_save_reboot.py` now reproduces the basic save lifecycle on disposable
copies of the current image for either PAL or NTSC. Both standards pass on
ADF `823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3`:

- A normal Courtesy run produces a generation-1 save with 191 best-time ticks.
- Only reserved sector 1738 changes; save identity and CRC validate.
- A fresh emulator restores the save from disk, leaving the saved disk unchanged.
- Visual inspection of both reboot captures confirms `003.18` in the menu.
- The factory ADF remains byte-identical. PAL and NTSC saved copies are also
  byte-identical for this deterministic trial.

Reports: `TRACKLOADER_SAVE_REBOOT_PAL_RESULTS.json` and
`TRACKLOADER_SAVE_REBOOT_NTSC_RESULTS.json`. This revalidates basic best-time
persistence after the PC-source asset change. Completion/achievement triggers,
interrupted writes and physical-machine behaviour are not covered by this trial.

## Current-image cold-boot save recovery

The all-PC image `823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3`
passes four native recovery scenarios on both PAL and NTSC:

| Fixture | Restored best ticks | Generation | Completion flag | Achievement bits |
| --- | ---: | ---: | ---: | --- |
| Both slots valid | 4501 | 2 | 1 | 0x12345678 |
| Newest slot CRC corrupted | 3901 | 1 | 1 | 0x12345678 |
| Both slots have foreign identity, valid CRCs | 0 | 0 | 0 | 0 |
| Generations 0xffffffff and 0 | 4501 | 0 | 1 | 0x12345678 |

`check_save_recovery.py` reads the actual game's records, generation and opaque
achievement field from RAM after cold boot. All other profile records remain
zero. Every fixture disk and the factory image stay unchanged. Reports:
`TRACKLOADER_SAVE_RECOVERY_PAL_RESULTS.json` and
`TRACKLOADER_SAVE_RECOVERY_NTSC_RESULTS.json`.

These tests seed disk sectors before boot. They verify native recovery decisions,
not physical interrupted writes, and do not implement or validate achievement
triggers. The initial test harness assertion was corrected to exclude Copperline's
control-interface startup line from the serial protocol comparison; game code
and production disk bytes were not changed.

## Current-image tune read failure and retry

The all-PC factory image now passes wrong-disk/reinsertion and mid-read disk-swap
retry sequences on both PAL and NTSC. Each injected failure emits
`NATIVE-TUNE-FAIL`; restoring the correct image and retrying emits the expected
`NATIVE-TUNE-READY` for Otis or Courtesy. All four tests finish within their
bounded emulator runs and leave the production image byte-identical.

Reports are the `OFS_REINSERT` and `OFS_MIDREAD` variants of
`TRACKLOADER_OFS_SWITCHING_RESULTS`, with `_NTSC` for NTSC. These are silent,
read-only media tests, not writable swaps or physical floppy removal tests.

`TRACKLOADER_CURRENT_STATUS.md` now collects nine current-image reports for
audio, saving, recovery and read failures. Its generator rejects mismatched
image hashes; historical reports remain in this chronological record.

## Repeatable PC source-audio preparation

`make trackloader-audio PC_MUSIC_DIR="/path/to/data/music"` now prepares all
three PC banks with the established 250 KiB, preboosted Fibonacci settings.
It verifies all source hashes before writing outputs and requires final AUD1
files to match the existing accepted lock exactly. The lock is never refreshed
implicitly. `AUDIO_PYTHON` selects the NumPy/SciPy environment.

The PC source selector accepts an explicit music directory, removing its need
for a cached executable-provenance report. Expanded PC auditions no longer
read legacy PCM trials or preview WAVs that they do not use. The optimal encoder
creates its output directory when absent.

Both the regular build and an isolated build with no audio cache, legacy
previews, or PC executable provenance cache regenerate all three accepted bank
hashes exactly. The isolated build receives only audio tools, the lock verifier,
three analysis JSON files, PC verification/comparison JSON, the asset lock and
an empty docs directory. Reports: `TRACKLOADER_SOURCE_AUDIO_BUILD_RESULTS.json`
and `TRACKLOADER_SOURCE_AUDIO_CLEAN_RESULTS.json`.

This closes the historical-audition dependency for source audio. It does not
prove a full clean-checkout ADF build or automate cue extraction/toolchain setup.
Production bank bytes and the existing ADF are unchanged.

## Isolated source-to-ADF build

An isolated snapshot of the working-tree sources now reproduces the validated
ADF byte-for-byte: `823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3`.
The workspace began with no generated music, SFX or trackloader outputs.
Source-audio preparation, Zultra packaging, target inflate/relocation checks,
native compilation, disk assembly and save-manifest generation all completed.

This found and removed two cached dependencies:

- `build_release.py` now prepares SFX from source OGGs and checks ffmpeg/source
  prerequisites before compiling the game.
- `make_save_manifest.py` no longer reads the historical reserved-layout report.
  It and the image builder use `native_layout.py`, preserving the exact on-disk
  ABI and the existing allocation/ownership checks.

The snapshot initially omitted repository `*.inc` source files; those were added
before compilation succeeded. No generated game data was copied to compensate.
The isolated build uses repository cue assets, the installed SDK/vasm/ffmpeg,
and the local execram source checkout. Dependency installation, cue extraction
and a committed clean-checkout test remain outside this proof.

Report: `TRACKLOADER_ISOLATED_BUILD_RESULTS.json`. The production ADF and save
identity are unchanged; existing current-image emulator results still apply.

## Current-image writable save retry, PAL and NTSC

The enhanced `check_writable_retry.py` passes on both standards for the current
all-PC image. It now samples the actual game `save_dirty` flag and six best
records, rather than relying only on final disk contents. The 191-tick record
stays pending through the foreign-disk failure and reinsertion; generation 1
then stores that exact record in sector 1738. The retry key also starts another
game, resulting in a later generation-2 record in sector 1749 (237 ticks PAL,
359 NTSC). Both generations remain CRC-valid and carry the current identity.
At the final observation, the pending flag is clear and the latest record in
RAM matches a valid slot. Only the two reserved save sectors changed; the foreign
disk and factory image stayed byte-identical.

The first stricter assertion incorrectly required the final record to equal the
pending record even after that second game. The corrected test requires the
pending record to remain unchanged through reinsertion, verifies its persisted
slot, permits subsequent improvement, and verifies the improved slot as well.
No production game code or disk assets changed.

Reports: `TRACKLOADER_WRITABLE_RETRY_RESULTS.json` and
`TRACKLOADER_WRITABLE_RETRY_RESULTS_NTSC.json`. Precise swap timing relative to
write DMA is not instrumented here; the instantaneous writable-swap emulator
issue and physical-drive validation remain separate outstanding items.

## Standalone PC cue extraction

`make trackloader-cues PC_GAME_BIN="/path/to/SuperHexagon"` extracts all three
cue tables without cached executable provenance, audio previews, NumPy, SciPy
or an analysis rerun. The extractor verifies the supported executable SHA-256,
derives literal positions from the verified RIP-relative instructions, applies
the PC Otis scale, and checks all three results against the accepted asset lock
before writing anything. Other executable revisions are rejected.

Extraction to an empty output directory reproduced all repository cue bytes.
A deliberately unsupported executable was rejected without creating output.
Report: `TRACKLOADER_CUE_EXTRACTION_RESULTS.json`. Production assets and ADF
remain unchanged. This removes cue extraction from the outstanding build tasks;
installed dependencies and a committed clean-checkout build remain separate.

## Automated isolated rebuild gate

`make test-trackloader-isolated` now repeats the isolated source-to-ADF proof
in one command. It verifies source inputs, creates a fresh source snapshot,
excludes cached cues and all generated output directories, and runs cue
extraction, bank encoding, payload packaging and release assembly. It compares
the complete ADF with the reference and verifies the reference remained unchanged.
Source snapshot hashing and retained workspace/log paths make failures reviewable.

The first full run passed with no cached cue, audio, SFX or trackloader outputs:
`823eafb45c162c2cf0d4f23b20852238bf41286cd7cccc189b22e39ec42fedd3`.
Report: `TRACKLOADER_ISOLATED_BUILD_RESULTS.json`. Unlike the preceding manual
snapshot trial, this run also regenerates cues rather than copying them.
The command tests the working tree, not a committed Git checkout, and still
requires installed dependencies and the matching licensed PC inputs.

## Animated loading and saving activity

Track loads and saves now show a grey rail with a moving white segment below
`LOAD TRACK` / `SAVING`. It is an indeterminate activity indicator, not a
percentage or time estimate. A private level-3 VBlank handler updates only
copper WAIT coordinates; gameplay, audio processing and the gameplay frame
counter do not run in this handler. Resident tune loading permits only this
VBlank interrupt during disk reads, inflate and Fibonacci expansion. Save
transactions use the same display/handler and restore the normal interrupt
vector afterward. Both playfield colours match along the rail so old geometry cannot mask
the indicator, while bitplane timing remains enabled for sprite text.

PAL and NTSC control-protocol tests observe the activity counter advancing
through the load while the gameplay frame count remains fixed, then confirm
the normal handler is restored. Screenshot inspection confirms the rail and
moving segment are visible. This does not add a display during the initial
boot-block/resident startup before the game HUD exists.

New ADF: `4a3caf9be38c094d889c5cafc226c1a6ab15debc0b5605a2ea2fe4db1250d193`.
Stage size is 22,389 bytes; 60 disk sectors remain free. Older exhaustive
validation belongs to the earlier image and must not be attributed to this
build. `summarize_release_checks.py --allow-stale` now explicitly labels those
reports as needing reruns while its default still rejects stale evidence.

Final-image checks pass: PAL/NTSC loading animation and IRQ restoration,
PAL/NTSC writable save and cold reboot, and NTSC Courtesy/Otis/Focus/Courtesy
switching. Host storage/retry tests pass with the new display lifecycle mocked
and balanced. Reports `TRACKLOADER_LOADING_ACTIVITY_{PAL,NTSC}_RESULTS.json`
include observed busy/game frame counters. Final PAL and NTSC screenshots were
visually reviewed for readable text and a clean activity rail. Hardware tests
and the older exhaustive media-failure cases remain outside this change's checks.

## Memory-aware soundtrack preload and cache

Startup now loads Courtesy before entering the menu, followed by Otis and Focus
when decoded-bank memory is available. Each slot reserves 500,000 contiguous
bytes. The original reclaimed A500 slow-RAM bank remains supported; additional
banks are allocated through Exec before OS takeover, preferring non-Chip RAM
and falling back to Chip RAM. Overlapping slow-RAM allocations are rejected.

One slot retains the active tune; two retain the two most recently used tunes;
three retain the whole soundtrack set. Cache misses replace the least recently
used bank. Hits restore bank metadata and rebind the small resident cue stream
without reading the floppy, inflating the bank or decoding Fibonacci samples.
Failed replacements remain invalid and retryable. Courtesy is made active again
after preloading. Startup therefore takes longer with extra slots, while later
track changes avoid repeated loading. The loading activity bar remains active
during preload.

`make test-tune-cache` checks hits, profile aliases, eviction, failed loads and
repeated recency updates. `tools/trackloader/check_tune_cache.py` checks actual
boot allocation, startup contents and repeated track selections in emulator RAM.

Cache validation passes on PAL A500 emulation with one, two and three slots
(0, 512 KiB and 2 MiB additional Fast RAM respectively). All preload counts
match available memory. Repeated Otis/Courtesy/Focus/Courtesy/Otis selections
confirm one-slot replacement, two-slot least-recently-used eviction and all-hit
three-slot operation. Host cache and storage/retry tests also pass.

Built ADF: `out/trackloader/332e5d41cefeab58003b0a6fb4ac787b22587958b7dbb77daa0a14c848bc8abe.adf`.
Stage size is 22,521 / 22,528 bytes; game memory is 195,772 / 196,608 bytes;
59 disk sectors remain free. Reports are `TRACKLOADER_TUNE_CACHE_{1,2,3}_PAL_RESULTS.json`.
These checks do not replace NTSC, physical hardware or media-failure validation
for this image; the current-status page marks older-image evidence explicitly.

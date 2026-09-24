# Bootable trackloader implementation plan

Status: working native implementation on `trackloader`; see
[the current validation summary](TRACKLOADER_CURRENT_STATUS.md) for current-image
evidence and remaining limits. `TRACKLOADER_FEASIBILITY.md` records the history.
The baseline notes below describe the starting point, not current capabilities.

## Objective and baseline

Boot a single DD ADF directly through its boot block, take control from AmigaOS
early, and retain filesystem access through the supplied disk code. Load a
soundtrack when its level family is selected; persist progress, achievements
and best times. Compress immutable disk payloads with zultra and decompress
with kierf's inflate using a verified overlapping-buffer layout.

Initial hardware target: stock 68000 A500, 512 KiB Chip + 512 KiB expansion,
PAL and NTSC. Retain the existing DOS build as a separate diagnostic target.
Accelerated machines, especially the previously problematic 68060, need an
explicit cache/vector compatibility check before being declared supported.

Repository evidence:

- `DosIO.s` exposes load/save/delete/list/format and requires $4d00
  (19,712) Chip bytes; it includes `diskio.s`. Fix include case for portable
  builds rather than linking a second copy of DiskIO.
- `DiskIO.s` exposes sector read/write and requires $3300 Chip bytes.
  It uses CIA-B timer A, requires master DMA, and does not use the blitter.
- `DosIOR.s` is a read-only alternative, insufficient for persistence.
- Supplied routines are predominantly encoded `dc.l` instructions, including
  mutable state; disassemble them to establish interrupt, cache and ABI needs.
- Current startup opens libraries, allocates memory, installs interrupts and
  restores the OS on exit. This cannot be reused unchanged after permanent
  takeover.
- `PcRecords` stores six best times and six completion flags, currently only
  for the session. Achievement definitions/storage are not yet established.
- Latest Fibonacci audition packages total 760,097 bytes, before zultra,
  game files or filesystem overhead. Decoded sample banks are about 490–494 KB
  per tune. Neither disk fit nor peak-memory feasibility is yet established.

## 1. Prove the compression, disk and RAM budgets first

1. Locate and pin the exact zultra tool and kierf inflate source/version.
   Confirm stream framing, supported DEFLATE block types, entry ABI, scratch
   requirements, alignment, bounds/error behaviour and CPU requirements.
   Existing execram zultra usage is evidence of a build dependency, not proof
   that its stream or depacker matches the new boot loader.
2. Replace audition JSON containers with a versioned binary tune layout:
   sample rate/count, entry offsets/lengths, sequence, cue data, codec ID and
   checksums. Keep the accepted Fibonacci encoder and current source gains.
3. Compare actual zultra sizes for (a) the Fibonacci tune package and (b) its
   fully decoded PCM bank. Choose using measured disk and peak-RAM costs.
   Path (a) requires a separate Fibonacci decode after inflate; inflate alone
   does not produce PCM. Keep Fibonacci decoding out of the gameplay loop.
4. Specify and verify overlap for BOTH decompression stages independently.
   Source-at-end is not automatically safe: establish a proven bound or measure
   the maximum source/output separation for each exact stream with instrumented
   decoders and target guard checks. Store required slack in build metadata.
   If kierf's implementation cannot support the needed overlap, resolve that
   before implementing the loader; use bounded staging only if RAM permits it.
5. Produce a linked memory map covering boot/resident code, vectors, stacks,
   game, SFX, display/sprites, disk workspace, music, all decoder scratch and
   compressed input. Budget startup and tune replacement separately. Release
   the old tune before loading a different one. Reuse workspace only across
   phases whose lifetimes are proven disjoint; keep DMA data in Chip RAM.
6. Build a sector-accounted size report for the 901,120-byte ADF, including boot
   blocks, filesystem metadata, file block overhead, save reservations and
   alignment. Fail builds that exceed capacity or overlap memory arenas.

Exit gate: one disk and the 1 MiB machine fit with explicit scratch/stack
allowances. If not, report the measured deficit and alternatives before changing
sound quality, persistence requirements or supported hardware.

## 2. Establish the filesystem and read/write harness

- Add assembler wrappers with a documented C ABI and error translation. Serialize
  calls through one workspace. Audit CIA, custom-register, interrupt-mask and
  mutable-code accesses; the wrapper must restore the game's hardware state.
- Confirm the filesystem variant actually supported by these routines; start
  with the OFS/DOS0 layout suggested by the embedded constants, then verify it
  in a disposable disk harness. Do not assume FFS support.
- Implement deterministic image construction with contiguous file data/block
  lists where supported, validated against real DosIO reads. Preserve original
  loader attribution and record source/version provenance.
- Bound every read with manifest size/allocation checks; DosIO has no destination
  capacity argument. Validate directory/file chains before invoking unrestricted
  loads, or add bounded sector-based loading around the supplied DiskIO routine.
- Harness operations: read and compare files, create/load a temporary save,
  overwrite, read back, and check disk removal/write-protection/error handling.
  Only use disposable ADF copies for write tests.

Exit gate: filesystem read and write work after disabling OS services, with
no unintended changes outside the test file's allocated sectors.

## 3. Boot block and resident bootstrap

- Generate a checksum-correct 1,024-byte boot block and a small resident second
  stage. Keep first-stage sector placement explicit in the image manifest.
- Use only the minimal available boot-time services to identify/reserve memory,
  identify CPU/video standard and fetch the resident stage. Do not open DOS or
  start Workbench. Define the exact takeover boundary.
- Before takeover, establish supervisor stack, memory arenas, vector ownership,
  PAL/NTSC information and CPU/cache handling. After takeover, issue no Exec,
  DOS or graphics-library calls. Document which boot buffers remain live.
- Resident stage owns DiskIO/DosIO, inflate, hardware initialization and a
  minimal visible error/loading screen. Load/decompress the game with an
  explicit entry point, BSS clearing and Chip/ordinary section placement.
- Prefer a small explicit section/relocation format generated from the linked
  game over depending on an OS HUNK loader. Validate each relocation and bound.
- Give boot mode a reset/reboot exit path; do not call the DOS build's restore
  and library-close path after OS memory has been reclaimed.

Exit gate: cold boot reaches a diagnostic screen, loads/decompresses a checked
payload and returns to resident control without an OS call after takeover.

## 4. Separate the game platform layer

- Introduce boot/DOS platform adapters for allocation, timing/video detection,
  startup, exit and file access. Move allocations before display startup and
  replace late OS allocations with the pre-budgeted arenas in boot mode.
- Preserve existing triple-buffer ownership, early-VBlank display publication,
  input behaviour, simulation timing and PCM fades.
- Audit all linked support/library routines for hidden OS calls, including
  allocation, debugging, exceptions and startup helpers.
- Establish disk ownership of CIA-B timer A and the disk DMA/interrupt bits.
  During initial integration, disk I/O runs only in an explicit loading/saving
  state with audio stopped and simulation paused. Use a static copper display;
  determine whether VBlank/input can safely remain enabled from the loader audit.
- On exit from I/O, restore hardware and reset elapsed-time baselines so loading
  does not advance simulation, music fades or records by the stalled duration.

Exit gate: existing gameplay tests and PAL/NTSC boot smoke tests pass with no
filesystem activity or decompression in the active gameplay loop.

## 5. Load and switch soundtracks

- Load the selected base stage's tune before confirming the run. Normal/Hyper
  variants share a bank where PC mapping confirms this. Verify mapping and
  per-track cue/timing data from the existing PC references.
- Transition: fade/stop audio, wait for DMA/ISR ownership to end, release old
  bank, load and validate packed file, inflate, Fibonacci-decode if applicable,
  apply the existing gain once, bind PCM sequence/cues, reset clocks, start run.
- Retain the current tune on retry or same-family selection; no unnecessary
  rereads. Loading failures stay in a recoverable menu/error state, with retry
  and disk-insertion handling. Do not start a run with a partial bank.
- Expose a runtime PCM-bank binding interface instead of compiled-in incbins.
  Retain PCM copying during gameplay, along with PAL/NTSC periods, fades and
  cue synchronization. Test full-song loops and seeking.
- Record track-load time, peak RAM and first-audible-sample behaviour. A loading
  pause is expected; no uninterrupted asynchronous gameplay loading is promised.

Exit gate: every level selects its correct tune, switches repeatedly without
leaks/corruption, and maintains the current gameplay performance.

## 6. Persistent records and achievements

- Define a versioned endian-explicit save format with magic, payload length,
  generation, six best times in simulation ticks, completion flags, achievement
  bits and checksum. Derive unlocks from completion flags as today. Specify the
  achievement list/triggers from verified PC behaviour or user requirements;
  do not invent rewards or change the three initially unlocked levels.
- Keep gameplay records in RAM and mark them dirty. Save at the results/menu
  boundary after audio is stopped, coalescing updates rather than writing each
  improved tick. Display write-protect/failure status and retain dirty state.
- Preallocate two fixed-size save slots when building the disk. Put them on
  separate physical tracks: a torn track write must not damage both generations.
  Protect their allocations in the filesystem bitmap and validate their identity.
- Prefer bounded data-sector updates through DiskIO after resolving the slots,
  avoiding DosIO's potentially non-atomic file replacement and bitmap updates.
  If OFS data blocks are used, preserve their headers and recalculate checksums.
  Leave allocation and directory metadata unchanged during saves.
- Write the inactive slot with generation+1 and checksum, then read it back and
  validate. Only then regard it as current; keep the older slot intact. At boot
  choose the newest valid slot with wrap-safe generation comparison. Define
  default/migration behaviour for missing, corrupt and older-version saves.
- Saving is optional on write-protected media; loading and gameplay must work.
  Verify disk identity before writes so a swapped disk cannot receive game saves.

Exit gate: progress, achievements and records survive cold reboot, and interrupted
writes recover either the previous or new complete state without damaging assets.

## 7. Build and validation gates

Add separate `trackloader`/`trackloader-adf` targets with isolated object/output
paths. Emit a reproducible disk manifest, payload hashes, compression tool
versions, sector map and peak-memory report. Never overwrite a user's save ADF
as part of ordinary asset regeneration; tests start from fresh disk copies.

Host checks:

- Binary packages round-trip through independent decoders with exact lengths;
  validate inflated executable/data hashes and Fibonacci output against previews.
- Overlap stress: incompressible, highly repetitive, boundary-sized and actual
  payloads; truncated/corrupt headers, lengths and checksums; output canaries.
- Image boot checksum, file chains, bitmap, sector ownership and save separation.
- Persistence versioning, corrupted slots, interrupted writes at each write
  boundary, sequence wrap and disk identity checks.

Target checks (Copperline and FS-UAE, plus physical hardware when available):

- Cold boot PAL/NTSC A500 512+512, full game and all three tunes, repeated switching,
  retry, full-song loop, no audio underruns and before/after gameplay FPS.
- Disk absent, swapped, write-protected and failed read/write; no unbounded hang.
- Save, power-cycle, recover unlocks/best times/achievements; interrupt a save and
  confirm recovery. Verify emulator write persistence explicitly.
- 68060 startup, loader mutable-code/cache handling and decompressed-code cache
  visibility; do not infer physical compatibility from a stock 68000 test.

## Suggested reviewable milestones

1. Compression/overlap proof and disk/RAM budget report.
2. Disk filesystem harness with verified read/write behaviour.
3. Bootable resident loader and platform abstraction, diagnostic game launch.
4. Full game with dynamic tune loading, sound/cue validation and timing results.
5. Transactional persistence with agreed achievement definitions.
6. Final single-disk image, reproducible build instructions and validation report.

Resolve before dependent implementation: exact kierf inflate source/version,
proven overlap strategy, measured single-disk/peak-RAM feasibility, and the
achievement definitions. None prevent beginning the budget and loader audits.

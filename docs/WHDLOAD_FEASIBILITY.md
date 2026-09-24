# WHDLoad feasibility investigation

Investigated and implemented 2026-09-24. The separate WHDLoad launcher and
file-save backend now build with `make whdload`. See `WHDLOAD_RESULTS.json`
for the exact runtime checks and remaining validation limits. The following
design records the integration choices; hardware compatibility is not implied.

The recommended approach is to reuse the native game's relocatable package,
audio decoder, cache and hardware renderer, and provide WHDLoad-specific file
loading, memory setup, persistence and exit. A boot-sector wrapper alone would
retain assumptions that do not belong in this environment.

## Existing integration points

- `trackloader/game_boot.h`: `TrackGameBoot` already supplies the heap, video
  mode, tune loader, decoded-bank arenas, metadata and save callbacks.
- `main.c`: native startup preloads as many tunes as it receives arenas for.
  Escape from the title returns from the game, providing a natural exit hook.
- `trackloader/tune_cache.c`: one to three decoded banks, 500,000 bytes each;
  retains recently used tracks and replaces the least recently used on a miss.
- `tools/trackloader/package_native_game.py`: produces EXE1 with relocation
  records and explicit memory size. Reuse its validator/relocator.
- `trackloader/tests/native_stage.s`: contains reusable decode/bind sequencing,
  but also physical disk access, serial diagnostics and a terminal halt loop.
  Extract the reusable logic instead of duplicating the whole stage.
- `trackloader/native_save.c`: safe-point snapshots, retry and acknowledgement
  can be retained; its floppy-sector transaction backend needs an alternative.

The latest build occupies 195,772 bytes of its 196,608-byte game arena. Its
22,521-byte resident stage has only seven bytes spare in the floppy reservation.
Keep WHDLoad-specific code outside those fixed floppy limits.

## WHDLoad interface constraints

Use `resload_LoadFile` for payloads, `resload_SaveFile` for saves and
`resload_Abort` for exit. Filenames must be relative: remove `DF0:` prefixes.
Keep our inflater: WHDLoad's documented decrunch formats do not include raw
DEFLATE. Flush CPU caches after inflating and relocating executable code.
WHDLoad owns VBR/MMU/cache registers; do not modify them directly or call Exec
through address 4. Expansion memory comes from the slave's declared allocation,
not the boot loader's fixed `$c01000` reclaim. Optional ExpMem is all-or-nothing;
version-20 memory configurations provide selectable alternatives, not a general
runtime allocator. These constraints are documented in the
[official API](https://whdload.de/docs/autodoc.html).

Use the existing low-vector interrupt interface with a zero logical vector base;
verify WHDLoad forwarding and all installed IRQs rather than passing its private
VBR into the game. Remove unnecessary floppy-motor manipulation in this backend.
Audit loading IRQs across host I/O transitions.

WHDLoad `PRELOAD` caches files, whereas our cache holds decoded audio. Both can
help, but they consume separate memory budgets. Missing preload capacity can
cause host OS transitions; do not promise a continuously animated loading bar
while host I/O is active. See [options](https://www.whdload.de/docs/en/opt.html).

## Proposed memory configurations

These are design budgets, not measured total machine requirements:

| Configuration | Base/Chip reservation | Expansion reservation | Decoded tunes |
| --- | --- | --- | --- |
| Small | 512 KiB | 512 KiB | 1 |
| Medium | 512 KiB | 1 MiB | 2 |
| Full | 512 KiB | 1.5 MiB | 3 |

Initially reserve the current 360,448-byte native workspace inside BaseMem,
clear it explicitly and leave low vectors untouched. Supply slices of ExpMem
as bank pointers. Put additional slave code outside the native game arena.
Validate actual stack/workspace bounds before freezing the layout.

Use explicit small/medium/full variants initially, or selectable WHDLoad 20
memory configurations. Do not claim automatic three-to-two-to-one fallback until
it has an implemented, tested selection mechanism. Retain the existing preload
order Courtesy, Otis, Focus and MRU retention policy.

WHDLoad itself requires Kickstart 2.0 and at least 1 MiB RAM; our game reservations,
WHDLoad/OS overhead and file preload require further capacity. A practical first
test target is an A1200 with 2 MiB Chip and 4 MiB Fast, followed by constrained
configurations. See [requirements](https://www.whdload.de/docs/en/need.html).

## Payloads and saves

The implementation keeps the exact accepted PC-derived Fibonacci assets and
Zultra-compressed tunes. It stores the game EXE1 package uncompressed on hard
disk to avoid unnecessary startup decompression. Package separate game and tune files plus a manifest with hashes,
lengths and decode bounds. This avoids OFS parsing and physical disk services.
An ADF-backed adapter is possible, but adds little value when source is available.

Preserve the existing save record codec, CRC, generation ordering and snapshot
acknowledgement. Store alternating records in two files. Restore the newest valid
record and acknowledge a snapshot only after successful persistence. Test missing,
truncated, corrupt and unwritable files. Keep factory payloads immutable. Define
save identity/version policy explicitly: the current payload-derived identity
changes with builds, so compatibility across future releases is not automatic.

Initially reuse the current pending-save UI lifecycle with a backend-neutral
commit callback. Do not simulate floppy anchor checks for ordinary files.
Preserve failed pending snapshots. Explicitly handle a dirty save on normal exit;
a forced WHDLoad quit may bypass the game's save path.

## Implementation and acceptance sequence

1. Add WHDLoad SDK include paths, slave assembly and a separate packaging target.
   Load/relocate the game, initialize one tune arena, enter the title and exit
   cleanly to Workbench. Keep the native ADF target unchanged.
2. Implement the tune callback with file loading and existing decode/bind code.
   Test all tracks and six profiles, then one/two/three-slot cache operation.
3. Add file saves and cold-relaunch recovery, failure retry and normal-exit flush.
4. Package the launcher icon, PRELOAD configuration, payload manifest and README.
5. Test PAL/NTSC; 68000 and accelerated 68020/030/060 configurations; quit during
   title/play/loading; cache settings; low memory; PRELOAD on/off; read-only saves;
   and repeated launches returning to a usable Workbench. Use WHDLoad memory and
   hardware diagnostics. Run existing host regressions and rebuild the ADF.

Cache coherency and Chip DMA must be verified on accelerated machines; start with
conservative settings and use WHDLoad's cache controls. Its
[cache documentation](https://www.whdload.de/docs/en/cache.html) explains why
BaseMem and ExpMem have different defaults. Existing PAL cache tests do not
establish WHDLoad or 68060 compatibility.

The repository now has `whdload/slave.s`, shared resident loading code, a
file-journal backend, launch icons and a separate packaging target. The developer
SDK and user-provided AmigaOS media remain external dependencies. The existing boot-ADF test harness alone cannot
validate Workbench restoration or WHDLoad filesystem persistence.

## Implemented build and validation

`make whdload` produces `out/whdload/Decahexagon.zip`, containing the three
slaves, original project icons, Shell launch scripts, payloads and manifest.
The minimum slave version is WHDLoad 20. The poll-based keyboard uses NoKbd;
launchers use NOWRITECACHE so safe-point saves reach the host filesystem.
Normal exit retries pending writes and reports failure without acknowledging
an unsaved snapshot. Saves use a stable WHDLoad schema identity, separate from
the payload-derived native-floppy identity.

The final one-slot slave was run on PAL 68000, the final two-slot slave exercised
protected-save failure on NTSC 68060, and the final three-slot slave was launched
through its Workbench icon on PAL 68060 and returned to the desktop. Earlier
three-slot PAL 68020 and two-slot NTSC 68060 trials cover all tunes, cache misses,
successful journal writes and restoration; their distinct hashes are retained
in `WHDLOAD_RESULTS.json` rather than presented as final-binary evidence.

Host journal/cache/storage tests pass. The native ADF rebuild remains byte
identical to SHA-256 `332e5d41cefeab58003b0a6fb4ac787b22587958b7dbb77daa0a14c848bc8abe`.
Physical hardware and exhaustive WHDLoad diagnostic/endurance coverage remain
open. See `whdload/README.md` and `tools/whdload/prepare_test.py --help` for
build and reproduction instructions. No AmigaOS files, ROMs or SDK files are
redistributed in the archive.

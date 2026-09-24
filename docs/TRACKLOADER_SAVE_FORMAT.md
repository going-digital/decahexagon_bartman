# Native save format, version 1

Implemented by `trackloader/save.c`. This is a RAM codec and two-slot selector;
no disk writes or game-state import/export are connected yet.

Each slot is exactly 512 bytes. Integers are unsigned big-endian. Reserved bytes
must be zero. The format is independent of C structure padding and host endian.

| Offset | Bytes | Meaning |
| --- | --- | --- |
| 0 | 4 | Magic `HXS1` |
| 4 | 4 | Version, 1 |
| 8 | 4 | Slot length, 512 |
| 12 | 4 | Generation, modulo 2^32 |
| 16 | 16 | Trusted disk/build identity |
| 32 | 24 | Six best scores in simulation ticks, normal profiles then Hyper |
| 56 | 4 | Completion mask, bits 0–5 only |
| 60 | 4 | Opaque achievement bits; triggers not yet defined |
| 64 | 444 | Reserved, zero |
| 508 | 4 | CRC-32/ISO-HDLC over bytes 0–507 |

CRC uses reflected polynomial 0xEDB88320, initial 0xFFFFFFFF and final complement,
matching Python `zlib.crc32`. This detects accidental damage, not deliberate edits.
Completion flags retain their current game meaning; unlocks are derived through
`pc_profile_unlocked`, preserving three initially unlocked normal levels. No
achievement triggers or score limits are invented by the codec.

## Recovery contract

Decode validates the entire slot before modifying its output. Encode validates
completion flags before writing. Buffers must not overlap. The selector returns
slot 0 or 1 when at least one valid slot can be chosen. It returns -1 if neither
is valid, leaving the output unchanged; the caller should use zeroed defaults.
Unknown/older versions are rejected; no migration exists yet.

When both slots are valid, unsigned generation subtraction chooses the newer
slot across wraparound, provided their distance is less than 2^31. A distance
of exactly 2^31 is ambiguous and returns -2. Equal generations choose slot 0
only when all bytes match; conflicting contents return -2 without changing the
output. Future integration must surface this conflict and avoid automatic writes
that discard either copy.

The future writer must update the inactive slot, increment generation modulo
2^32, read back and validate the complete bytes before considering it committed.
The two slots must occupy different physical tracks. The existing disk identity
field does not prove the currently inserted disk is safe to write: trusted media
identity/layout verification must precede writes. Failures must retain dirty RAM
state and the prior committed slot.

## Validation

- `make test-track-save`: round-trip all fields, all 512 single-byte corruption
  positions, all 513 prefix lengths of a torn inactive-slot replacement, blank
  slots, identity mismatch, conflicting/ambiguous generations and wraparound.
  Torn writes always select either the previous committed state or the complete
  new state. Tests do not simulate arbitrary physical track damage.
- `python3 tools/trackloader/check_save_format.py`: independent endian/CRC fixture
  and malformed headers with recomputed valid checksums; rejected decode leaves
  output unchanged. Results: `TRACKLOADER_SAVE_FORMAT_RESULTS.json`.
- Codec compiles for m68000 with warnings treated as errors. This is compile
  verification, not target execution or cold-boot persistence validation.

## Game-state boundary

The game now exposes these storage hooks in `game.h`:

- `game_restore_save`: accepts a decoded state once, immediately after
  `game_init` and before any `game_update`. It updates records, generation,
  opaque achievement bits and the selected profile's displayed best. Invalid
  completion flags or a late/dirty restore are rejected without changes.
- `game_save_dirty`: reports unsaved record/completion changes. A new session
  begins with zeroed records; unsuccessful loading leaves those defaults.
- `game_save_snapshot`: exports dirty state only in the menu or full results
  mode. Many improved ticks coalesce into one snapshot. The proposed generation
  is the last acknowledged generation plus one, modulo 2^32.
- `game_save_committed`: call only after disk write/readback verification. A
  matching snapshot clears dirty state. If records improved after the snapshot,
  acknowledgement advances the generation but leaves dirty state set. Duplicate
  or stale generation acknowledgements are rejected. A failed/write-protected
  write must not call this function.

These are synchronous main-thread APIs, not interrupt-safe callbacks. The storage
adapter still needs to stop audio, verify disk identity and slot ownership, write
the inactive slot, verify readback and then acknowledge. No caller currently
performs those disk operations. The existing ADF has not been rebuilt for this
step; host tests exercise the real game boundary functions and the native game
sources link without OS imports. Run `make test-game-save test-track-save`.

## Raw-slot transaction layer

`trackloader/save_disk.c` accepts synchronous uncached sector read/write callbacks
and a trusted media manifest. It checks three immutable sector snapshots, reads
both slots and chooses the inactive slot using the codec. Before the write it
rechecks identity; afterward it compares the entire readback to the pending bytes,
decodes it and checks identity again. Only this complete sequence returns success.
It never calls the game acknowledgement itself. Both invalid nonblank slots or
an unexpected generation return conflict without writing, except an exact
already-committed snapshot can be acknowledged again after fresh verification. Two zeroed slots permit
initial generation 1 in slot A.

Raw slot A is sector 1738 on physical track 158; B is sector 1749 on track 159.
They are separate from the old disposable OFS `save_a`/`save_b` test files and
from filesystem metadata. The adapter must preserve the other ten sectors of the
target track when DiskIO performs its track update. Its cache must be invalidated
for verification reads, and it must abort on media changes during the transaction.
Identity comparisons alone cannot prevent a disk being physically swapped between
a check and a write.

`tools/trackloader/make_save_manifest.py` validates the build hash, complete OFS
layout, reserved-track ownership and blank factory slots, then generates
`scratchpad/trackloader/native_game/save_manifest.json`. It contains exact sector
snapshots for the first boot sector, root and bitmap, plus a build-specific identity
derived from the executable and soundtrack payload hashes. This metadata must be embedded as
trusted resident data, not constructed from arbitrary inserted media at runtime.
Cross-build save migration is not implemented.

`make test-save-disk` tests identity refusal, blank initialization, inactive-slot
writes, unchanged prior slot, stale generations, write failure, mismatched readback,
post-write identity change and corrupt-slot refusal using mock sector callbacks.
The transaction source compiles for 68000, but is not linked to native disk I/O.
A reported failure after a completed write can still leave a valid newer slot;
future retry/recovery integration must reread/reconcile that state rather than
blindly advancing or reusing the old generation. Dirty gameplay state must remain
pending until that reconciliation succeeds.


## Startup reads and verification-failure retry

`track_save_disk_load` checks identity, reads both slots, selects the newest valid
state and rechecks identity before publishing any output. Two blank slots return
`TRACK_SAVE_DISK_BLANK` with zero defaults. Other successful recovery returns
`TRACK_SAVE_DISK_OK`. Corrupt/conflicting slots, identity mismatch and I/O errors
leave the caller's output unchanged. One valid slot survives damage to the other.
The eventual boot adapter may pass a successful result to `game_restore_save`
before the first simulation tick; no boot caller is connected yet.

Commit retry now accepts an exact snapshot already present in the freshly read
selected slot, after checking identity again. It performs no write. This resolves
a write which completed but whose readback failed, without falsely treating a
different same-generation state as committed. If RAM improved since that snapshot,
the caller must retry/acknowledge the original pending snapshot first and then
obtain a new generation from the game API. It must not substitute newer contents
at the old generation.

`python3 tools/trackloader/check_save_image.py` runs the actual C transaction on
a disposable copy of the verified native ADF using host sector callbacks. It
writes generations 1/2 to the two reserved sectors, reopens the image with fresh
RAM state, recovers generation 2, retries it without writing, and verifies fallback
to generation 1 after corrupting the newest slot. Every sector outside the two
slots and the source ADF remain unchanged. This is storage-layer recovery, not
an emulator reboot or hardware write test. Evidence is recorded in
`TRACKLOADER_SAVE_IMAGE_RESULTS.json`.

## Native startup integration

The boot stage now reads sectors 1738/1749 using the original DiskIO, stops the
motor after each read and calls the relocated game's retained `track_save_select`.
Each slot is read independently; failed reads are discarded before selection. Valid selection publishes an optional `TrackSave` pointer
in the extended `TrackGameBoot`; `main.c` restores it immediately after game init
and before any update. Read failure, unsupported/corrupt/foreign slots or ambiguity
leave default records. This read-only path performs no disk writes or identity-anchor
checks for write authorization. Future write integration must distinguish conflict
from blank defaults and must not overwrite rejected saves automatically.

The two raw slot buffers reuse stage offsets 38000..39024. Decoded state is at
55000 and copied into game-owned RAM before later OFS scratch use can overwrite it.
The executable entry info at 54000 is preserved. The payload-derived 16-byte
identity is embedded in the resident stage before disk construction; the builder
then generates the matching trusted anchor manifest. This avoids a circular
identity dependency on the stage containing its own identity. Identity changes
with executable or tune payload changes; cross-build migration is still absent.

`make_startup_save_fixtures.py` produces three write-protected disposable images
using an independent Python encoder: latest valid generation (75.01 seconds),
damaged newest with older fallback (65.01 seconds), and foreign identity (zero
defaults). PAL A500 emulator boots verify these displayed values and the serial
restore/default markers. These are externally seeded saves, not game-written ones.

## Native write integration and end-to-end verification

`trackloader/native_save.c` connects the game snapshot/acknowledgement boundary to
`track_save_disk_commit`. It coalesces changes until menu/results, retires pending
render work, stops music/SFX, masks interrupts and uses a static SAVE banner during
blocking I/O. Afterward it restores display/interrupt ownership and main rebases
the simulation clock. A failure shows SAVE ERROR and retains both dirty state and
the exact pending snapshot. It does not retry every frame; another run rearms it.
On successful retry an older pending snapshot is acknowledged before a newer dirty
snapshot is attempted. Saves remain optional on write-protected media.

The boot ABI now provides a raw transfer callback, identity and trusted anchors.
The builder assembles the stage a second time after constructing immutable disk
metadata, embedding compressed boot/root/bitmap snapshots. Startup inflates the
1548-byte anchor blob to stage +45000, disjoint from DiskIO workspace, track cache,
inflate scratch and decoded startup state. Save transaction scratch resides in
the Chip-resident executable BSS.

The raw adapter accepts reads within the disk and writes only to 1738 or 1749.
It bypasses the tune cache and turns the motor off after each transfer. A media
sequence reset precedes identity checks; subsequent transfers reject an asserted
DF0 disk-change signal. This supplements identity checks but does not establish
safety under removal during a physical track write. That case remains untested.
The original DiskIO code performs the partial-sector track update. Emulator disk
comparison confirms other sectors of both save tracks are preserved.

PAL A500 emulator tests on a disposable writable disk completed the full cycle:
first run wrote generation 1 / Hexagon 414 ticks (6.90 seconds) to slot A; a reboot
and Hexagoner run wrote generation 2 / 139 ticks (2.31 seconds) to slot B while
preserving the Hexagon record. A further cold boot displayed 2.31 in the Hexagoner
menu. Only sectors 1738 and 1749 changed. A separate write-protected run displayed
SAVE ERROR and left its disk unchanged. These are game-written saves, superseding
the earlier seeded/host-only checkpoints above.

Evidence: `TRACKLOADER_NATIVE_SAVE_RESULTS.json`; recheck the captured artifacts
with `python3 tools/trackloader/check_native_save_result.py`. The writable trial
is `scratchpad/trackloader/native_game/save_write.adf`; the standard native ADF
configuration remains write-protected. No physical-hardware, NTSC, power-loss or
mid-write-removal validation has been completed. Achievement triggers and
cross-build save migration remain undefined/unimplemented.


## Save scratch placement and code-size settings

The save transaction's 2048-byte scratch now lives in the existing resident Chip
block at +46800..48848, passed explicitly through `TrackGameBoot`. It is disjoint
from trusted anchors (+45000..46548) and inflate scratch (+49072..52000), so loading
and saving cannot overwrite each other's retained data. The native module checks
for a nonnull, even-aligned pointer; a compile-time size assertion and builder
region checks guard the contract. No extra Chip allocation is introduced.

Native save modules and boot validation/glue use `-Os`. Frame/render/IRQ code and
tune expansion/decoding retain `-O2`. This reclaims space without changing formats
or gameplay rules; loading-time performance is not claimed identical.


## Interrupted-write and unreadable-track recovery

`make test-save-interruption` exercises the real transaction across 513 written
prefix lengths, both inactive-slot orientations and two transport outcomes
(failure or reported success): 2052 cases. The active slot is always unchanged;
recovery returns the entire previous or next state, never mixed records. A reported
successful partial write must still pass readback before commit can return OK.

A recovery defect found by these tests is fixed: a read error on one track no
longer suppresses a valid save on the other. Both the C storage loader and native
startup discard partial output from failed reads and try the other slot. If no
valid slot remains, the C API reports I/O error (not blank factory media) and
preserves its output. Writes still require both slots to be readable; recovery
does not authorize overwriting an unreadable track.

`make_unreadable_save_fixtures.py`, run after `make_startup_save_fixtures.py`, emits
write-protected UAE-1ADF images with track 158 or 159 replaced by raw 0xAA MFM,
containing no valid sector headers/sync. The other 159 tracks are normal data.
PAL A500 emulator boots log NATIVE-SAVE-READ-FAIL, then restore the other slot:
75.01 seconds when A is unreadable, 65.01 when B is unreadable. This exercises an
actual DiskIO read failure, not only save-CRC rejection. Fixture hashes remain
unchanged. Evidence: `TRACKLOADER_SAVE_INTERRUPTION_RESULTS.json`.

The raw-track fixtures model the aftermath of damage. They do not test timed
power removal during DiskIO writes, drive mechanics or real hardware.

### Media-change boundary regression coverage

`make test-save-media-change` injects 84 transfer-boundary changes across commit
and load, both slot orientations, and foreign or identity-identical replacement
media. With a latched-change adapter, errors preserve the active save and startup
output; retry of the retained snapshot either writes it or acknowledges its
already completed write without duplicating the generation. Failed reads may
modify scratch, so the test also verifies that such output is never published.
This host model does not validate CIA timing or media removal within DiskIO.

The native adapter's media-change contract is not yet established for changes
inside a DiskIO call. `TRACKLOADER_DISK_CHANGE_RESULTS.json` reproduces a modeled
change latch being cleared by seeking before the driver's check. Thus passing
the boundary-model tests is insufficient to claim replacement-media protection.
This remains an open native-save gate; post-write identity validation cannot
undo a write to the wrong medium.

Write-guard control flow is now covered by
`tools/trackloader/check_disk_write_guards.py`: eight actual-68000 trials verify
pre-write rejection and retry suppression, plus stack restoration and drive
cleanup. The preceding MFM read is stubbed successful and DMA is forced to time
out; this does not establish physical-write atomicity. See
`TRACKLOADER_WRITE_GUARD_RESULTS.json` for results.

The native derivative additionally polls disk-change status while waiting for
DMA and disables DMA on change before error unwinding. The write probe now has
14 cases, including change simultaneous with modeled completion and ordinary
successful completion. It checks DMA is disabled on every return. This prevents
accepting the modeled changed-media completion, but cannot undo already-written
bytes or establish physical-write atomicity.

The native retry harness now uses the real codec and sector transaction layer.
It verifies write-protection failures, loss of readback after a completed write,
idempotent acknowledgement on retry, and subsequent generation-2 storage of newer
progress in the alternate slot. Sector hardware and gameplay callbacks remain
mocked; the real selector validates recovery of the latest result.

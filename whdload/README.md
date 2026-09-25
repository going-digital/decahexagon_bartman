# Hexagon WHDLoad development build

Requires WHDLoad 17 or newer. The declared minimum is based on an API audit;
recorded emulator runtime checks currently use WHDLoad 20. Older versions still
need startup, save/load and quit testing.

Copy this directory to an Amiga hard disk with WHDLoad installed. From its Shell:

    WHDLoad SLAVE=Hexagon-3.slave PRELOAD NOWRITECACHE QUITKEY=89

Double-click Hexagon-1, -2 or -3 in Workbench, or execute Run-1, Run-2 or
Run-3 from the Shell. All variants request 512 KiB Chip
BaseMem; their expansion reservations are 512 KiB, 1 MiB and 1.5 MiB respectively.
These are game reservations, not total machine requirements. Allow additional RAM
for AmigaOS, WHDLoad and PRELOAD. There is no automatic memory fallback.

Courtesy loads first; additional slots preload Otis and Focus. The decoded cache
retains most recently used tracks. PRELOAD is a separate packed-file cache.
The supplied icons/scripts explicitly select F10 (QUITKEY=89), overriding a
global Escape quit key which would bypass saving. When updating, replace the
launcher icons/scripts too, while retaining existing save files.
Escape from the title exits normally; F10 is the WHDLoad quit key where supported.
Normal exit retries pending saves and reports failure if they cannot be written;
forced quit may discard unsaved progress.

The supplied launch commands use NOWRITECACHE so acknowledged saves reach the
host filesystem rather than waiting in WHDLoad until exit. This can briefly
switch back to the OS at save time.

Progress is stored in data/save-a and data/save-b using alternating CRC-protected
records. Do not copy native ADF save sectors here: this backend has its own stable
schema identity. Missing files start fresh. Corrupt or unwritable saves are not
silently acknowledged. Keep the data directory writable or configure WHDLoad's
SavePath as appropriate. Payloads are never rewritten.

The three PC-derived tunes retain the accepted preboost/Fibonacci encoding.
The game EXE1 package is stored uncompressed for hard-disk startup. This build
requires no embedded Kickstart image and does not access the physical floppy.

Emulator checks cover PAL 68000/68020/68060 and NTSC 68060 startup, gameplay,
save recovery, protected saves, PRELOAD-off I/O and normal OS return. Exact
per-build evidence is recorded in the repository at docs/WHDLOAD_RESULTS.json.
Physical hardware, full-song endurance and exhaustive accelerator/cache options
remain unverified. This is a development build, not a hardware certification.

Build from the repository with `make whdload WHDLOAD_SDK=/path/to/WHDLoad` after
`make trackloader-adf` has generated and verified the shared payloads. The SDK is
the official WHDLoad DEV package; its files are not included in this source tree.
Use WHDLoad 20.0 for the current validation baseline.

`make test-whdload test-tune-cache test-trackloader-storage` runs host regressions.
`tools/whdload/prepare_test.py --help` describes how to create a private FS-UAE
hard-drive test from your Workbench/modules ADFs and matching ROM. Supply saves
with `--saves` for a cold-recovery test, `--no-preload` to exercise host reads,
and `--video NTSC` or `--cpu 68060` for compatibility checks. OS files and ROMs
stay in ignored scratch directories and are never included in the game package.

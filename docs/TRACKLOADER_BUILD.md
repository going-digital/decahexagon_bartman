# Trackloader build

From the project root, rebuild the native game and publish a factory ADF:

```sh
make trackloader-adf EXECRAM_SOURCE=/absolute/path/to/execram
```

`make trackloader` is an alias. To regenerate the soundtrack banks from the
matching PC recordings before packaging and building:

```sh
make trackloader-cues PC_GAME_BIN="/path/to/SuperHexagon"
make trackloader-audio PC_MUSIC_DIR="/path/to/PC/data/music" AUDIO_PYTHON=venv/bin/python
make trackloader-assets EXECRAM_SOURCE=/absolute/path/to/execram
make trackloader-adf EXECRAM_SOURCE=/absolute/path/to/execram
```

The audio step requires NumPy, SciPy, ffmpeg and a host C compiler. It verifies
all three source hashes before decoding and checks the resulting banks against
the accepted lock. It never updates that lock automatically. A mismatched
source is rejected; no PC executable or historical audition cache is needed
when `PC_MUSIC_DIR` is supplied. The default audio interpreter is
`venv/bin/python`; override `AUDIO_PYTHON` for another environment.

An isolated working-tree source snapshot now reproduces the complete ADF
byte-for-byte without cached audio, sound effects or trackloader outputs. This
uses the repository cue assets and installed toolchains. Cue extraction is now
available separately through `trackloader-cues`, with exact-hash verification
against the supported PC executable. Dependency installation and a committed
clean-checkout test remain separate tasks.

To regenerate the Zultra library, compressed payloads and target inflate proofs
from the accepted preboosted Fibonacci AUD1 banks first:

```sh
make trackloader-assets EXECRAM_SOURCE=/absolute/path/to/execram
make trackloader-adf EXECRAM_SOURCE=/absolute/path/to/execram
```

The preparation target expects `{courtesy,focus,otis}_expanded_fibonacci.aud`
in `scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/`. These are the
approved 250 KiB-budget, preboosted banks using all three PC sources. It does not
re-encode the original recordings or regenerate the audition banks. Python
numerical/audio packages are not required for this packaging step. Zultra vendor
sources must be present under the supplied execram checkout.

The release build regenerates sound effects from `assets/sounds/*.ogg` using
ffmpeg. The native builder and save-manifest validator share the fixed sector
layout in `native_layout.py`; no historical trial-disk layout report is required.
The builder checks that reserved stage/save sectors are unoccupied.

Prerequisites:

- Python 3, a host C compiler and `vasmm68k_mot` on PATH.
- The Bartman Amiga GCC SDK under the user's VS Code extensions directory
  (the existing scripts currently select the macOS SDK).
- A local execram checkout containing Musashi and `stubs/inflate/inflate_core.s`.
- `scratchpad/trackloader/libzultra.dylib` and the prepared Courtesy, PC Otis and
  Focus `*.fibonacci.deflate` payloads, plus `assets/music1.cues` through
  `assets/music3.cues`.
- Existing payload measurements and tune inflate proofs
  under `docs/`. The native image builder verifies the payload hashes against
  these reports; it rejects mismatches rather than accepting stale proofs.

The command checks prerequisites, links the native game without OS imports,
packages it, checks independent relocation results, rebuilds the 68000 inflate
harness, runs target relocation/inflate checks and assembles the ADF. Intermediate
objects remain isolated under `scratchpad/trackloader`; the ordinary game build
is not used as an object cache.

Output is `out/trackloader/<full-sha256>.adf` with a matching JSON manifest.
The manifest records stage/game size, asset hashes and save identity. Published
factory images have blank save slots. An existing hash-named image is accepted
only if its bytes still match; a modified image is never overwritten. Use a copy
for writable gameplay. The intermediate `native_menu.adf` remains a regenerable
factory artifact, not a place to keep saves. A rebuild does not touch named
emulator save-test copies.

Run the host storage regression suite separately:

```sh
make test-trackloader-storage
```

This includes codec, disk transaction, interrupted-write, media-change,
game-boundary and native retry tests. Emulator tests are separate and require a
local Kickstart ROM; see `TRACKLOADER_FEASIBILITY.md` and the individual scripts
in `tools/trackloader`. PAL and NTSC A500 emulator checks are recorded separately. The factory build
does not claim physical-drive or 68060 compatibility. The documented instantaneous writable-swap emulator failure
also remains open.

The first build through this entry point reproduced SHA-256
`96017d688d3749bf07cda0f30160e38943500d43699c3b29eae46bcb9a0d2460`, matching the
image used for the PAL A500 save/retry/DMA-removal trials at that point. Stage size is
22,289 bytes (239 spare); game memory is 194,036 bytes (2,572 spare).

The release command also rebuilds `dosio_probe` and `dosio.bin` itself after
Musashi source generation. It does not require the earlier filesystem experiment
or its `game_budget` executable. The historical filesystem trial shares the same
writer-build helper.

Asset preparation validates all three AUD1 headers, preboost/rate settings,
Fibonacci block lengths, PCM offsets and sequence bounds before writing any
library, payload or proof output. To run only that preflight:

```sh
python3 tools/trackloader/prepare_payloads.py \
  --execram-source /absolute/path/to/execram --check-only
```

Accepted soundtrack identity is recorded in
`soundtrack/trackloader_assets.lock.json`. Asset preparation checks the three
AUD1 source banks before writing output and checks the resulting compressed
payloads/cues afterward. Release builds check the compressed tunes and cues
before compiling. The release manifest includes the lock-file hash. This
prevents a structurally valid replacement bank or a cue change from silently
changing the accepted soundtrack. The lock is never regenerated automatically;
intentional audio changes require reviewing and updating it along with the
relevant source/audition evidence.

`make test-trackloader-assets` validates the current nine assets and verifies
that modified or missing copies are rejected without touching project inputs.

The latest native stage auto-detects PAL/NTSC from the raster. Initial boot and
Courtesy-load smoke tests pass in both standards; complete NTSC audio/storage
validation remains pending. This stage is 22,375 bytes (153 spare), producing
`fd37804a4aa739736305c710aefe6f049e8745f8efaac6c1e2ecd148d17b12ca.adf`.
The earlier hash above identifies the prior PAL-only build's validation history.

The auto-detecting image also passes NTSC writable saving and cold restoration
of a 3.18-second result (`TRACKLOADER_NTSC_SAVE_RESULTS.json`). NTSC audio timing,
all-track switching and media fault coverage remain separate outstanding gates.

NTSC all-track switching and wrong-disk/mid-load recovery now pass as well:

```sh
python3 tools/trackloader/check_native_switching.py --video NTSC --rom /path/to/kickstart.rom
```

These use read-only media and silent emulation. See
`TRACKLOADER_OFS_SWITCHING_RESULTS_NTSC.json`; audio timing and write-time fault
validation are not implied by these loader checks.

`check_ntsc_player.py --rom /path/to/kickstart.rom` reads live video timing fields
and audio block/underrun counters through the emulator control interface. Four
short NTSC playback runs pass with zero underruns; full-song loops, waveform/cue
alignment and physical-hardware performance remain unverified.

`check_ntsc_loop.py --rom /path/to/kickstart.rom` is an explicitly modified-RAM
endurance diagnostic: it bypasses collisions only in the emulator session.
Courtesy exceeds one full sequence length with continuous DMA and zero underruns;
the disk image remains unchanged. See `TRACKLOADER_NTSC_LOOP_RESULTS.json` for
patch details and limits. This is not an unmodified gameplay or audio-quality test.

The endurance harness also supports `--track otis` and `--track focus`. Both now
pass the same full-sequence, zero-underrun NTSC diagnostic as Courtesy, with
separate reports and unchanged disk images. The collision-bypass and waveform/
physical-hardware limitations still apply.

After running PAL/NTSC switching and the three NTSC soundtrack endurance
checks on the current factory image, validate their combined evidence with:

```sh
python3 tools/trackloader/check_pc_audio_release.py
```

This verifies all-PC source hashes, zero cue leads, current asset identities,
runtime song lengths and continuous DMA counters. It refuses reports from an
older ADF. These silent emulator tests do not establish listening quality or
physical hardware compatibility.

To test basic record persistence on disposable copies of the current image:

```sh
python3 tools/trackloader/check_save_reboot.py --video PAL --rom /path/to/kick13.rom
python3 tools/trackloader/check_save_reboot.py --video NTSC --rom /path/to/kick13.rom
```

Each trial starts from blank save slots, plays Courtesy, verifies a CRC-valid
save with the current payload identity, then starts a fresh emulator and checks
restoration. Only the reserved save sector may change. The published factory
image is never written. Reports record the source image hash and evidence paths.

To check cold-boot selection of seeded save slots:

```sh
python3 tools/trackloader/check_save_recovery.py --video PAL --rom /path/to/kick13.rom
python3 tools/trackloader/check_save_recovery.py --video NTSC --rom /path/to/kick13.rom
```

This reads actual game records from emulator RAM after boot. Cases cover newest
valid generation, corrupt-newest fallback, foreign-identity rejection, and
32-bit generation wraparound. Best times, completion flags and opaque achievement
bits are checked. Fixtures are disposable and read-only; these are recovery
checks, not physical power-loss or achievement-trigger tests.

For writable save retry after replacing and reinserting the disk:

```sh
python3 tools/trackloader/check_writable_retry.py --video PAL --rom /path/to/kick13.rom
python3 tools/trackloader/check_writable_retry.py --video NTSC --rom /path/to/kick13.rom
```

The test uses disposable writable images and reads the game's pending-save flag
and best times from RAM. It requires the failed save to remain pending, the same
record to reach a CRC-valid save slot after retry, and the pending flag to clear.
The retry key also starts another game; any subsequent improved record must
be present in the other slot, preserving the original retried record.
The foreign disk and factory image must stay unchanged. The precise DMA phase
of the swap is not instrumented; this does not replace interrupted-write tests.


`trackloader-cues` uses only Python's standard library and the verified PC
executable. All three extracted tables must match the accepted asset lock before
any cue file is written. An unsupported executable is rejected. To verify without
writing, use `python3 tools/audio/extract_pc_cues.py --binary /path/to/SuperHexagon
--check-only` (on one command line). No cached audio-analysis results are needed;
repository verification metadata identifies the supported executable revision.

To reproduce the current reference ADF from a fresh working-tree source snapshot:

```sh
make test-trackloader-isolated \
  PC_GAME_BIN="/path/to/SuperHexagon" \
  PC_MUSIC_DIR="/path/to/data/music" \
  EXECRAM_SOURCE="/path/to/execram"
```

This copies source files into a new scratch directory, excludes generated cues
and all output caches, and runs cue extraction, music encoding, payload packing
and the release build. It requires exact agreement with the current reference
ADF and never overwrites it. Build logs and the isolated workspace are retained
for inspection. Use `--expected-adf` with `check_isolated_build.py` directly to
compare with another reference image. This checks the working-tree snapshot,
including uncommitted changes, rather than a Git checkout.

### Soundtrack cache checks

`make test-tune-cache` runs host tests for decoded-bank reuse and eviction.
For boot and menu-switch checks, run `tools/trackloader/check_tune_cache.py`
with `--rom /path/to/kick13.rom` and one of these memory configurations:

| Arguments | Expected decoded slots |
| --- | --- |
| `--fast 0 --slots 1` | Courtesy preloaded; later tracks replace it |
| `--fast 512K --slots 2` | Courtesy and Otis preloaded; least-recently-used eviction |
| `--fast 2M --slots 3` | All three preloaded |

The emulator checks use 512 KiB Chip plus 512 KiB slow RAM, read-only disposable
ADF copies, and report the image hash with observed cache contents. Extra memory
is allocated before OS takeover; actual available contiguous blocks determine
the slot count. More slots increase boot loading time but avoid repeated tune
reads and sample decompression when changing levels.

## Combined distribution

Build both editions and publish one ZIP with:

```sh
make dist EXECRAM_SOURCE=/path/to/execram WHDLOAD_SDK=/path/to/WHDLoad
```

`make release` is an alias. Prepared soundtrack assets are required as above.
The default SDK path is `scratchpad/whdload/sdk/WHDLoad`.
The result is `out/Hexagon.zip`, which extracts into `hexagon/` and contains
the factory ADF, the WHDLoad
installation directory (all three cache variants), instructions and manifests.
The two builds run sequentially before packaging. The previous ZIP is replaced
only after the new package passes integrity checks. Personal saves and stray
files in the output directories are excluded. Individual `trackloader-adf` and
`whdload` targets remain available for development.

# PC accuracy checks

`make test` compiles the portable core using the host C compiler. The same
`core_checks.c` is linked into an Amiga build with `PC_CORE_SELFTEST=1`.
A target build draws **PASS** or **FAIL** on the game bitplane after executing
those checks on the emulated/real 68000. This verifies arithmetic and selected
mechanics. The 2D renderer and presentation remain transitional.

Build (`tools/build.sh` also locates a locally installed Bartman VS Code SDK):

```
tools/build.sh -B -j4 PC_CORE_SELFTEST=1 out/hexagon.adf
tools/run_fsuae.sh pal
```

For NTSC add `EXTRA_CFLAGS="-DTARGET_NTSC"` to the build and use `ntsc` as the
runner argument. `pal512` selects a 512 KB chip-only A500. The runner copies
the current ADF into a private scratchpad directory; ensure its display
standard matches the build. `FSUAE_BIN` and `FSUAE_KICKSTART` override the
local defaults. ROMs are not supplied by this repository.

Use `-B` when changing build flags because the original build shares object
paths between configurations. A normal build omits `PC_CORE_SELFTEST`.

`tools/fsuae_keys.applescript` accepts `start`, `left`, `right`, `back`, `screenshot`, `smoke`, or `quit` and
addresses only FS-UAE via macOS accessibility automation. Screenshots and logs
are saved under `scratchpad/fsuae/<configuration>/`. Manual keyboard/joystick
verification is still needed for latency and feel; synthetic core inputs do
not test the physical device pipeline.

The preserved `fixtures/pc_native_mechanics.txt` came from selected original
x86-64 routines in the locally owned desktop executable:

- SHA-256: `91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`
- Input: `0x100052980`; logic: `0x100056f50`; effects: `0x10002adb0`.
- Audit date: 2026-09-19. External graphics/audio side effects were stubbed.
- Collision tests use speed22, candidate30, previous90, slot0.

The live normal-Hexagon, Hexagoner and Hexagonest paths now use the PC-unit store, generator, selector,
collision-before-motion ordering and uninterrupted integer-tick morphs.
Fractional speeds/deltas, full stage transitions, death effects and camera/cue
response remain outside this implementation. Marker22 is stored as a camera
request but its presentation effect is not yet applied.

The clock intentionally targets nominal 60 Hz desktop play on both display
standards, not the original executable's variable-delta long-frame clamp.
PAL input is sampled per display frame and reused on the occasional second
logic tick; it cannot reproduce input changes between those two sample times.

## Wave generation checks

`make test` also compares all 9,159 saved original-machine-code outcomes
(3,053 ordinary integer RNG paths at speeds22,33,40). It checks ordered wall
records, delay, marker wait, spawn base and draw count. The fixtures share the
executable provenance above; their source audit is
`scratchpad/pc_verification/WAVE_REFERENCE.md`. These tests cover generation
from an empty store, not selection, interrupted morphs or full game traces.

`tools/build_wave_data.py` reproducibly factors `pc_wave_branches.jsonl.gz` into
`pc_wave_data.inc`: 45 implemented IDs, 379 decision/emission nodes and 24,870
bytes of immutable tables. Only C tables and an integer interpreter ship;
Python and expanded fixture files are host-only. The independent saved native
output is `pc_wave_native.txt.gz`, driven by `pc_wave_inputs.txt`.

`tools/build_wave_checks.py` extracts one native case per ID into
`wave_cases.inc`. Shared host/68000 tests compare ordered record digests and
metadata, then exercise marker consumption, first-free-slot reuse and explicit
capacity failure. This target subset supplements the exhaustive host comparison.
Target delay countdown uses the ceiling of a rational delay at integer-speed
`dt=1`; variable-delta equivalence has not been established. Runtime RNG must
return an integer in `[0,bound)` and have no wall-store side effects. Exact
seed identity with the desktop runtime is not promised.

## Selector and integration checks

`make test` compares 184,320 Hexagon/Hexagoner/Hexagonest selector outputs with original
machine-code traces in `pc_selection_normal.csv.gz`,
`pc_selection_hexagoner.csv.gz` and `pc_selection_hexagonest.csv.gz`, covering synthetic wave,
side, score, shape-counter and RNG-seed combinations. Inputs start at speed22,
rotation mode0 and no active morph; the PC routine increments score before
selection, which the probe reproduces. This is not a full-run oracle or a
claim that every synthetic state occurs naturally.

`tools/build_schedule_checks.py` selects 402 native cases spanning each observed
(stage, chosen ID, sides, speed) tuple. These run on the host and actual 68000, along
with native morph endpoint timing, marker wait/resume, projection truncation
and same-slot overlap checks. A self-test build also latches FAIL if the live
first wave's travel disagrees with its simulation clock during the first 80
ticks. The target test result remains visible after death/retry.

`normal_run_test` forces survival for 256 seeds through tick 10800, stopping at
the stage boundary. It checks capacity and uninterrupted morph assumptions;
these are stress runs of the new code, not differential desktop replays.
Observed: peak 48 records,6,826 marker-triggered morph requests, and
side counts4..6. Other side-count cases are covered synthetically.

The render adapter uses a separate sorted/merged radial span list. It never
changes the authoritative collision ordering. PC distance and width are each
truncated by5 before addition, and markers/out-of-side-range records are not
drawn. Screen scale, pulse, player shape and planar camera remain adaptations.
The selector intentionally continues normal-Hexagon behavior past tick 10800
until progression is implemented; that continuation is not desktop-accurate.

The target suite also checks full-width 68000 multiplication (700×128 and its
signed counterpart). The existing helper previously returned a truncated
16-bit product, making distant PC-unit walls wrap into view. These checks are
68000-only; a host PASS does not substitute for this assembly regression test.
`tools/build_morph_data.py` regenerates the integer render-arc table from the
source's two repeated binary64 +/-0.1 trajectories.

The clipper tests exercise one million large-coordinate segments on the host
and 512 on the 68000, including a reproduced corner near-miss that previously
escaped clipping. They verify both on-screen line endpoints and right-edge
fill-parity spans. Raw blitter entry now rejects off-screen endpoints before
updating fill bounds or calculating DMA addresses.

## Playable Hexagoner build

Hexagon remains the default. Hexagoner can be built separately while level
selection and hyper progression are still unfinished:

```
tools/build.sh -B -j4 program=out/hexagoner EXTRA_CFLAGS="-DBUILD_DEBUG=0 -DPC_START_STAGE=1"
```

This produces `out/hexagoner.adf` and `out/hexagoner_packed.adf`. Run the
unpacked PAL disk with:

```
FSUAE_ADF="$PWD/out/hexagoner.adf" tools/run_fsuae.sh pal512
```

`FSUAE_ADF` overrides the runner's default `out/hexagon.adf`. For target
checks add `PC_CORE_SELFTEST=1`; for NTSC also add `-DTARGET_NTSC`. Build
configurations still share object paths, so use `-B` when switching flags.
The title reads HEXAGONER, with the wave91 opener and stage1 speed/selection
rules. This build option is temporary and does not implement the desktop's
six-level menu, unlock rules, or 180-second hyper handoff.

There are now 402 shared native selector cases. All three stages also receive 256
forced-survival stress runs through tick10800: Hexagon peaked at48 records
with6,826 morph requests; Hexagoner and Hexagonest peaked at61 records with no morphs.
The latter is expected from its ordinary selection pool. These remain stress
tests, not desktop full-run comparisons.

## Playable Hexagonest build

```
tools/build.sh -B -j4 program=out/hexagonest EXTRA_CFLAGS="-DBUILD_DEBUG=0 -DPC_START_STAGE=2"
FSUAE_ADF="$PWD/out/hexagonest.adf" tools/run_fsuae.sh pal512
```

Stage2 uses wave92, turn rate9, and wall speeds35/40. Shared checks exercise
its one-time post-7200 clear/rotation event across consecutive ticks while a
wave delay is still running, plus wave-based flip requests. The planar display
supports rotation modes0..9 but does not yet implement the PC camera, flips,
palette/music transitions or their gating. Hyper-entry wave95 and full level
progression are not enabled by this build option.

## Optional held-key steering assist

The assist is **off by default**, independently of `BUILD_DEBUG`. Enable it
only in a development build:

```
tools/build.sh CHEAT_MODE=1 program=out/hexagon_cheat EXTRA_CFLAGS="-DBUILD_DEBUG=0"
FSUAE_ADF="$PWD/out/hexagon_cheat.adf" tools/run_fsuae.sh pal512
```

Hold the top-row **8** key during play. A small 8 at the upper right indicates
that the key is held. The assist overrides left/right while
held and immediately returns to ordinary input on release. It looks ahead
48 simulation ticks over existing walls, chooses a route toward a safe sector,
and uses the normal turn rate and collision handling. Route planning runs at
10 Hz to bound its 68000 cost; steering follows that route at 60 Hz. It is an aid, not
invulnerability: an already trapped player, future spawns or morph changes can
still defeat it. No timing, wall geometry, collision or RNG rules are bypassed.

For a release, use the dedicated target, which forces cheats off even if the
outer command or environment requests `CHEAT_MODE=1`:

```
tools/build.sh release
```

`cheat.c` is excluded from the disabled link. Preprocessor guards remove its
call, the input field, key state and raw-key handler. The generated
`obj/cheat_config.h` changes when the switch changes and invalidates all target
objects; **switching off needs no clean or `-B`**. Other build flag changes still
require `-B`. Do not reuse the development disk as a release artifact.

Every disabled target link runs `tools/check_no_cheats.py`, which fails if the
link map contains `cheat.o` or the ELF symbol table contains cheat symbols.
This supplements compile-time exclusion; it does not depend on runtime flags
or dead-code elimination. `make test-cheat` exercises obstacle avoidance in
both directions, non-mutating prediction, command exclusion, and preserved
lethal collision. For a macOS emulator smoke test, compile the host-only Swift helper, focus
FS-UAE at its title/game-over screen, and run it:

```
swiftc tools/fsuae_assist.swift -o out/fsuae_assist
osascript tools/fsuae_keys.applescript focus
out/fsuae_assist
```

It starts play, holds 8 for20 seconds, captures during the hold, releases 8,
and captures again. macOS event-posting permission is required. The helper
is never linked into the game. `System Events`' text `key down` did not sustain
the ordinary 8 key in our test; this helper posts explicit hardware key events.

Palette reference: `make test-palette` checks 21,900 native PC palette ticks and OCS quantization. See [PALETTE_REFERENCE.md](PALETTE_REFERENCE.md) for mapping, provenance and reproduction. Also included in `make test`.

Progression: `make test-progression` compares 2,048 native opening/handoff ticks,
then runs 384 forced-survival stress runs across all six launch profiles.
Twelve compact native cases also run under `PC_CORE_SELFTEST=1` on the 68000.
See [PROGRESSION_REFERENCE.md](PROGRESSION_REFERENCE.md) for provenance, exact
handoff ordering, build flags and remaining menu/ending work.

## Menu and session records

`make test-menu` (included in `make test`) compares 1,992 native cases and runs
shared host/68000 menu and record checks. All six profiles now share one runtime
selector. See [rules, controls, fixture provenance and remaining limits](MENU_REFERENCE.md).

For reliable held-arrow tests on macOS, compile
`swiftc tools/fsuae_menu.swift -o out/fsuae_menu` and run
`out/fsuae_menu right` (also `left`, `start`, `back`) from the repository root.
It focuses FS-UAE, holds the key for 80 ms and captures the resulting screen.
Instantaneous AppleScript arrow taps can fall entirely between input polls.

## Start and retry timing

`make test-lifecycle` (included in `make test`) compares 1,698 native snapshots.
Twenty-four boundary states also run in shared host/68000 checks. See
[start/retry rules and scope](LIFECYCLE_REFERENCE.md). The macOS menu helper's
`retry` action holds Space across a death/retry cycle and captures both states.

## Death movement

`make test-death` (also in `make test`) compares 17,280 native death ticks.
The shared host/68000 checks include 181 boundary cases. See
[death ordering, continuation behavior and limits](DEATH_REFERENCE.md).
`out/fsuae_menu death` captures a sequence after starting/retrying a run.

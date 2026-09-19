# Normal/hyper entry and stage handoffs

Reference: owned macOS x86-64 PC executable, SHA-256
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`.
This is the nominal dt=1 model, not variable-delta or Windows parity.

## Verified rules

| Selected profile | First wave | Initial wave counter | Speed | Turning degrees/tick | Palette |
|---|---:|---:|---:|---:|---:|
| Hexagon | 90 | 0 | 22 | 7 | 0 |
| Hexagoner | 91 | 0 | 24 | 7 | 5 |
| Hexagonest | 92 | 0 | 35 | 9 | 10 |
| Hyper Hexagon | 93 | 31 | 33 | 7 | 3 |
| Hyper Hexagoner | 94 | 51 | 33 | 7 | 6 |
| Hyper Hexagonest | 95 | 51 | 40 | 9 | 9 |

The hyper-entry flag overrides the first generated wave once. Subsequent waves
use the existing stage selector with an effective-score offset of +3600 ticks;
displayed survival time still starts at zero. Initial rotation is rerolled from
0/1 for normal stages0/1, 2/3 for normal stage2, and 4/5 for hyper, excluding
the prior mode. The wrapper retains the selected profile across handoffs.

When a stage0/1 effective score is strictly greater than10800:

1. Clear old walls, marker wait, delay and morph; restore six sides.
2. Set wave counter51, speed33/40 and pending hyper-entry; reroll rotation4/5.
   The shape counter and player angle are preserved.
3. **Run the outgoing selector on this same tick.** It emits wave93 for the
   stage0-to1 handoff or94 for stage1-to2, then advances the wave counter to52.
   Switching selector before emission incorrectly produces94/95 instead.
4. On subsequent ticks use the next selector, with7200 subtracted from the
   effective score for each completed handoff. Displayed survival time keeps
   increasing. Switching into stage2 also changes turning from7 to9 degrees.

Consequently a normal Hexagon run hands off at displayed tick10801, then18001;
a Hyper Hexagon run at7201, then14401. Normal/Hyper Hexagoner hand off at10801
and7201 respectively. These rules are independent of whether a wave was due:
a positive countdown does not postpone a handoff. Palette6/9 is selected on
handoff without resetting the palette interpolation phase.

## Source and executable checks

- `gameinput`0x100052980: entry branches and initial rotation/speed/wave setup.
- `restart`0x100029c40, `restarthyper`0x100029e00: reset boundaries.
- `changetostage`0x100029ea0: clearing and continuation setup.
- `gamelogic`0x100056f50: effective-score remapping, strict thresholds,
  outgoing-wave ordering, continuation flags and restored displayed score.
- `generatewave`0x10000d0d0: opening geometry, including first distances3950
  for93/94 and4375 for95. Source call offsets are fingerprint-specific.

`python3 tools/capture_pc_progression.py > /tmp/pc_progression_native.txt`
checks the executable hash and runs the original logic/generator in private
memory. Audio, score-service and palette calls are stubbed; ofRandom uses a
shared deterministic draw stream. Synthetic initial states follow the verified
entry setup; this probe does not execute the desktop menu/input startup path.
The original executable is never modified and the app is not launched.

The fixture contains2048 ordered ticks: six fresh profiles, both sides of each
handoff threshold, pending/no-delay cases, and the second handoff in a Hexagon
run, with16 RNG seeds per case. Comparison includes displayed score/profile,
continuation flags, opening IDs, wave/shape counts, speed, ceil(float delay),
turn rate, rotation, RNG count, wall count and an ordered wall-record hash.
These are bounded differential traces, not complete input-driven playthroughs.

`make test-progression` runs that comparison plus384 forced-survival runs
through all reachable handoffs, stopping when stage2 reaches effective7200.
Stress runs validate storage/morph/transition invariants, not full PC replay
parity. A12-case native subset also runs in the shared host/68000 self-test;
regenerate it with `python3 tools/build_progression_checks.py`.

## Playable builds and remaining scope

Until the six-level selection menu is implemented, choose a profile at build:

```
tools/build.sh -B -j4 program=out/hyper_hexagon EXTRA_CFLAGS='-DPC_START_STAGE=0 -DPC_START_HYPER=1' release
```

Use stage1/2 and distinct program names for Hyper Hexagoner/Hexagonest. The
normal Hexagon default remains `tools/build.sh -B -j4 release`. Force rebuilding
when changing flags because the Makefile still shares object paths.

The six-level menu, unlocks, per-mode records, exact menu/retry/death timing
and position, scripted ending, camera/cue effects and original audio changes
are not implemented by this batch. Hyper launch builds are development access
to those modes, not an unlock/menu implementation. Existing sampled normal
selectors are retained beyond the implemented continuation rules; do not treat
survival through the ending as verified PC behavior. Cheat mode remains a
separate optional build; all release targets force it off.

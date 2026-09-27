# Gameplay CPU and blitter profile — 2026-09-27

Measured the current cheat-enabled native ADF on Copperline's stock A500 OCS,
68000, PAL, 512 KiB Chip + 512 KiB Slow, no Fast RAM/JIT. Guest music/SFX
remain enabled; host audio is muted. These are emulator measurements, not
hardware timings or whole-level averages. Two short, different gameplay windows
were sampled, so their FPS must not be interpreted as fixed rates for each level.

| Window | Render calls / 99 complete video frames | Render FPS | Blitter busy | CPU bus stalls |
|---|---:|---:|---:|---:|
| Hexagon, late gameplay | 87 | 43.97 | 25.35% | 4.93% |
| Hexagonest, around 52 seconds | 35 | 17.66 | 13.17% | 3.10% |

Steering assistance was held throughout both captures. Its own instructions
account for 3.42% and 9.41% respectively; arithmetic helpers called by it are
reported separately. Therefore these are not cheat-free release performance
figures. Both windows still execute approximately 60 game updates per second.

## Where time goes

Percentages below are sampled self time relative to the emulated capture time.
They include instruction execution and associated bus waits; spin waits are
charged to their containing functions. They are not exclusive CPU-versus-DMA
budgets: the CPU and blitter overlap.

| Function/path | Hexagon | Hexagonest |
|---|---:|---:|
| Unsigned division helper | 9.34% | 9.41% |
| render_scene (including inlined wall work) | 8.47% | 9.69% |
| One-dot line setup/drawing | 7.58% | 6.58% |
| blit_line_mode (includes waiting for prior DMA) | 7.08% | 3.08% |
| Line clipping | 5.85% | 5.59% |
| Direction-to-coordinate multiplies | 4.37% | 5.39% |
| Span projection/sorting/merging | 3.93% | 7.29% |
| Forward PCM copying | 3.95% | 2.70% |
| HUD copper construction | 3.48% | 1.41% |
| Signed division helper | 2.06% | 2.04% |

The denser window issues about 40 line blits per render versus 19 in Hexagon.
It spends more CPU time preparing fewer frames; it does not saturate the blitter.
Blitter bus ownership is 12.82% / 6.50%, lower than its busy interval because
it cannot transfer on every slot. CPU stalls attributable to blitter-nasty are
3.07% / 1.48%. Idle bus slots are **not** idle CPU time: the CPU also performs
internal instruction cycles.

## Initial optimisation priorities (historical)

See the repeat-benchmark reassessment at the end for current priorities.

1. **Remove duplicate simulation-to-render conversion.** update_playing calls
   project_state immediately before collision and again after collision. Collision
   reads PcPlayer/PcWorld/morph directly, not gamestate's projected fields.
   Keep the final conversion; investigate caching segment-angle conversion while
   the morph is unchanged. This avoids work without reducing simulation rate.
2. **Specialise angle arithmetic.** pc_render_angle currently calls the general
   32-bit unsigned divide helper for degrees*65536/360. Valid 0..359 inputs fit
   a single 68000 DIVU quotient. Keep a fallback for the public function's wider
   input range and exhaustively verify identical results. The entire 9% division
   cost cannot be credited to this change: timing and assist code also divide.
3. **Reduce wall preparation cost.** project_spans insertion-sorts by sector and
   radius every render. Try per-sector buckets/sorted lists and cache repeated
   endpoint projections. Preserve unions, morph arcs, offscreen XOR-fill parity,
   and simulation record order; do not simply cull everything right of screen.
4. **Overlap more work with fill.** render_spokes immediately calls
   blit_line_mode, waiting for the fill. Move only buffer-independent preparation
   into that gap. Player fallback rendering can touch the same plane and cannot
   safely be moved there unconditionally.
5. **Measure assist-off separately.** The assist is a significant diagnostic
   workload in dense scenes. Optimise its prediction/cache separately from the
   renderer, and retain the original movement/collision semantics.

No production optimisation was applied during the baseline captures. Each proposed
change needs an isolated before/after capture and visual/correctness checks;
percentages above are ceilings on opportunities, not promised FPS gains.

## First constant-division changes

Implemented after the baseline captures:

- `pc_render_angle`: for normalised 0..359 degrees, use
  `degrees*182 + ((degrees*23302)>>19)`. This is exactly
  `floor(degrees*65536/360)` over that range. Generated 68000 code uses two
  word multiplies, SWAP and a three-bit shift, with no division helper on this
  path. Non-normalised inputs retain the original division.
- `pc_pulse_tick`: division by two becomes a shift; division by three for
  unsigned 16-bit magnitudes uses `muluw(value,43691)>>17`. Generated code
  uses MULU, SWAP and a one-bit shift. Explicit `muluw` is necessary here:
  this compiler otherwise expands the constant multiply into shifts/adds.
  Larger magnitudes retain the original division.

Validation: all 65,536 angle input bit patterns match the original expression;
pulse magnitudes 0..65,536 match for both signs and all three stages, including
the fallback boundary. `make test-pulse test-cue-offset test` passes. Both
changed translation units compile with the SDK's GCC for `-m68000 -Ofast`,
and their assembly was inspected. The native ADF has since been rebuilt and profiled as described below. The
combined WHDLoad/ADF distribution ZIP has not been rebuilt in this profiling pass.

Other constant divisions remain candidates, not blanket substitutions. In
particular, `/60` timer and `/200` sample-position inputs are 32-bit; applying
a reciprocal valid only for 16-bit inputs would silently corrupt long runs
or audio cue positions. The PAL/NTSC clock divisor is a runtime parameter.

## Reproduction and raw evidence

Local snapshots, screenshots, sample streams and bus/line-blit traces are under
`scratchpad/performance_current/`. `play.clstate` and `play2.clstate` are the
actual gameplay snapshots. Earlier `stage0`/`stage2` snapshots caught loading
and are intentionally excluded from results.

Capture each gameplay snapshot using:

```
copperline-ctl profile STATE --rom ROM --out PROFILE_DIR --frames 100 --format native
python3 tools/performance/summarise_profile.py PROFILE_DIR \
  --elf scratchpad/trackloader/native_game/game.elf --out SUMMARY.json
```

Use the ELF from the capture's build. The analysis locates its pc_turn bytes in
Chip RAM to determine relocation, maps sampled instruction PCs to text symbols,
counts render_game entries, excludes the initial partial video frame, and
counts line/fill/clear operations once by start frame/beam position/destination.
Full machine-readable summaries (with ELF SHA-256) are PROFILE_HEXAGON.json and
PROFILE_HEXAGONEST.json. Preserve the raw snapshots before rebuilding the ELF.

## Before/after constant-division measurements

Fresh cold boots of preserved baseline and rebuilt native ADFs use identical
scripted inputs: F1/F3 at 100 seconds, held 8 from 101 seconds, snapshot at
190 seconds. Configuration matches the baseline machine above. These are new
windows (Hexagon around 90 seconds, Hexagonest around 45 seconds), not repeats
of the earlier 127/52-second windows. Faster execution changes render boundaries;
these are matched input scripts, not identical machine-state snapshots.

| Window | Before FPS | After FPS | Rendered frames before → after | Unsigned helper before → after |
|---|---:|---:|---:|---:|
| Hexagon | 26.22 | 26.73 | 52 → 53 | 9.07% → 7.39% |
| Hexagonest | 13.11 | 13.61 | 26 → 27 | 9.50% → 7.71% |

Each window contains 99 complete video frames, about 1.983 seconds. FPS now
uses summed CCK lengths divided by 3,546,895 PAL CCK/second; the earlier
analyser used polled timestamps, which introduce boundary jitter. The initial
baseline table above retains its original approximate numbers. Blitter busy
changes from 18.70% to 19.23% in Hexagon and 10.82% to 10.94% in Hexagonest.
The observed render gain is 1.9% / 3.8%, but only one additional rendered frame
per short window: treat this as a modest positive signal, not a whole-level
or hardware FPS guarantee.

An independent Musashi benchmark runs the actual old/new C functions with
identical inputs and verifies every result against precomputed reference values.
It includes identical loop/call/result-check overhead and excludes DMA/IRQs:

| Routine | Before cycles/call | After cycles/call | Saving |
|---|---:|---:|---:|
| Angle conversion, every 0..359 input | 600.15 | 270.15 | 55.0% |
| Pulse /2, cues -120..120 | 805.45 | 281.62 | 65.0% |
| Pulse /3, cues -120..120 | 797.70 | 362.00 | 54.6% |

Both versions use native build flags `-m68000 -O2`, without LTO. These
measurements establish the arithmetic improvement independently of the short
frame-count windows. General division remains in timers, cue positions, collision
and assistance; removing every unsigned helper call was not the scope.

Evidence: `CONSTANT_DIVISION_PROFILE.json`, `CONSTANT_DIVISION_BENCHMARK.json`;
raw before/after captures and preserved baseline ELF/ADF are under
`scratchpad/performance_current/`. Reproduce instruction timings with:

```
python3 tools/performance/benchmark_constant_division.py \
  --execram-source /path/to/execram --baseline cbf1e4b
```

The native ADF build passed linking, relocation checks, in-place target checks
and boot-image validation, and both new gameplay captures reached their intended
levels. `Hexagon.zip` remains unchanged by this profiling pass.

## PAL/NTSC clock specialisation

`pc_clock_advance` now explicitly requires 50 or 60 Hz. At 60 Hz it returns
`display_frames`, retaining the fractional remainder. At 50 Hz it computes
`frames + floor(frames/5)` and carries the residual tenths plus the stored
remainder. Exact `floor(frames/5)` uses `MULU #52429`, `SWAP`, and a two-bit
shift for every unsigned 16-bit frame count. Generated native `-m68000 -O2`
code contains no division or modulo helper calls in this function.

All 65,536 frame counts, all 60 possible stored residues, and both rates were
checked against the original quotient and remainder (7,864,320 cases), including
residues carried across a rate change. `make test` passes and the changed source
compiles for 68000. This additional optimisation is not included in the earlier division comparison
above; its separate measurements follow below.

### Clock accuracy and performance results

Independent exhaustive host validation reports **7,864,320 cases, zero tick or
remainder mismatches**. It covers every uint16 frame count, both supported rates,
and all stored residues 0..59 (including PAL/NTSC transitions). Within this
contract there is no rounding error or accumulated timing drift. Actual 68000
execution also checks single-frame calls and boundary counts
0, 2, 5, 255, 32767, 65535 against precomputed tick/remainder tables.

| Clock update | Before cycles/call | After cycles/call | Reduction |
|---|---:|---:|---:|
| PAL, one frame | 1772.83 | 493.50 | 72.2% |
| NTSC, one frame | 1776.83 | 308.83 | 82.6% |

These Musashi 68000 instruction counts include identical test-loop, call and
result-check overhead; DMA and interrupts are excluded. Both use production
`-m68000 -O2` compilation without LTO. Roughly 1,279 PAL / 1,468 NTSC cycles
are saved per call in the single-frame trials; the clock is called per main-loop
iteration, so this does not translate to a 72–83% whole-game speedup.

Fresh before/after native ADF captures isolate **only** the clock change; the
before binary already has the angle/pulse optimisations:

| Window | Before FPS | After FPS | Render count | Unsigned helper time |
|---|---:|---:|---:|---:|
| Hexagon, around 90 seconds | 26.73 | 27.23 | 53 → 54 | 7.39% → 7.06% |
| Hexagonest, around 45 seconds | 13.61 | 13.61 | 27 → 27 | 7.71% → 7.63% |

Same configuration and input schedule as the preceding comparison; 99 complete
video frames per capture. The observed Hexagon gain is 1.9%; Hexagonest shows
no FPS change at this short window's resolution. Different render boundaries
slightly change sampled work, so this is a modest signal, not a guaranteed
whole-level gain. Blitter busy changes 19.23% → 19.24% / 10.94% → 11.29%.

The native ADF rebuild and its relocation/target/image checks passed; new
captures reach both intended gameplay scenes. The distribution ZIP is unchanged.
Evidence: `CLOCK_BENCHMARK.json`, `CLOCK_PROFILE.json`, and raw captures in
`scratchpad/performance_clock/` (including the preserved before ELF/ADF).
Reproduce accuracy and instruction timings with:

```
python3 tools/performance/benchmark_clock.py --execram-source /path/to/execram
```

## Remove duplicate simulation-to-render conversion

Removed the `project_state()` call immediately before `pc_collide()` in
`update_playing()`. Collision reads `PcPlayer`, `PcWall`, side count and speed
from simulation state, with no reads of projected `gamestate` fields. The
post-collision projection remains: it publishes the resolved player angle and
updates `gamestate.num_sides` before `patterns_tick()` consumes it. Projection
after a stage-transition morph reset remains necessary and is retained.

Normal gameplay now projects once instead of twice per tick, removing two
`pc_render_angle()` calls per tick (120 calls/second at 60 simulation ticks).
This is a work-count reduction, not a measured FPS claim. `make test
 test-game-loading test-game-save` passed, as did the native ADF build's
link, relocation, target and image checks. This ADF was subsequently boot-tested and profiled below; `Hexagon.zip` was not rebuilt. Before-build ELF/ADF copies are
preserved in `scratchpad/performance_projection/` for subsequent comparison.

### Duplicate-conversion accuracy and performance results

`tools/performance/check_projection_equivalence.py` extracts the production
`project_state()` and runs both the old projection/collision/projection sequence
and the new collision/projection sequence with production `pc_collide()`.
**2,488,320 cases, zero mismatches** in side count, segment angle, player angle,
previous player angle, hit flag and blocked flag. Cases cover all 360 angles,
sides 3..6, all 12 morph phases and both directions, eight collision-distance
boundaries, three widths and three speeds. Two walls exercise rollback and
subsequent collision checks; 483,840 cases block movement and 1,451,520 hit.
This is an exact check of the changed sequence, not a complete game trace.

The preserved before native ADF already includes angle, pulse and clock changes.
Both builds use identical cold-boot input scripts and the previous A500 PAL
configuration, with guest audio and held-8 assistance enabled:

| Window | Before FPS | After FPS | Rendered frames | Projection path time before → after |
|---|---:|---:|---:|---:|
| Hexagon, around 90 seconds | 27.23 | 27.23 | 54 → 54 | 1.473% → 0.737% |
| Hexagonest, around 45 seconds | 13.61 | 14.12 | 27 → 28 | 1.511% → 0.750% |

Projection path time sums self time in `project_state`, `pc_morph_arc` and
`pc_render_angle`, including associated bus waits. In the Hexagon window,
118 simulation updates execute 472 angle conversions before and 236 after:
exactly the intended halving. Hexagonest's window boundaries contain slightly
different update counts; its angle calls fall from 484 to 240.

The redundant work is demonstrably removed: projection CPU time roughly halves,
saving about 0.74–0.76 percentage points of total emulated time in these windows.
FPS is unchanged in Hexagon and rises 3.7% in Hexagonest, but that is only one
additional rendered frame in a 99-video-frame capture. Do not extrapolate this
short-window result to a guaranteed whole-level FPS gain.

Blitter busy is 19.24% → 19.45% / 11.29% → 11.34%; division-helper self time
is 7.06% → 7.14% / 7.63% → 7.54%. The removed conversions already used
multiplication, so this change does not directly remove division calls.

Evidence: `PROJECTION_EQUIVALENCE.json`, `PROJECTION_PROFILE.json`, raw captures
and preserved baseline binary in `scratchpad/performance_projection/`.
The new ADF boots into both intended gameplay scenes; distribution ZIP unchanged.

## Assistance-off investigation

Released held-8 in the existing populated gameplay snapshots, then captured
100 video frames. Both unattended players died during the capture, so full-window
FPS is rejected as a normal-play baseline. Excluding the first frame executing
`pc_death_tick` and all subsequent frames leaves 41 complete live Hexagon frames
and 50 Hexagonest frames. Neither retained sample executes `cheat_steer`.
These short, different scenes are exploratory samples, not an optimisation A/B.

| Path | Hexagon live sample | Hexagonest live sample |
|---|---:|---:|
| render_scene self | 12.01% | 11.70% |
| blit_line_onedot | 10.62% | 11.75% |
| render_clip_line | 9.01% | 9.46% |
| project_spans | 7.21% | 7.97% |
| direction_to_cartesian | 6.25% | 5.89% |
| blit_clipped_line_onedot | 5.51% | 6.19% |
| unsigned division helper | 5.85% | 5.99% |

The retained windows render at 26.79 / 20.97 FPS; do not compare these with the
prior two-second assist-on captures as a claimed speedup. CPU wall preparation,
clipping and line setup remain the principal targets without assist.

Before introducing a coordinate cache, reconstructed the ordinary span union
from each start-snapshot world and counted sector/radius corner keys:

| Snapshot | Merged spans | Corner requests | Unique corners | Repeated |
|---|---:|---:|---:|---:|
| Hexagon | 14 | 56 | 40 | 16 (28.6%) |
| Hexagonest | 22 | 88 | 80 | 8 (9.1%) |

These are world-radius keys before zoom rounding, not measured cache hit rates.
They show why a general coordinate cache should not be implemented blindly:
the dense scene has relatively little reuse and lookup/reset costs could erase
the benefit. Prefer benchmarking span sorting and clipping/line-setup changes
next; retain coordinate caching as a candidate requiring a target-cycle trial.
No renderer production changes were made in this investigation.

Evidence: `NOASSIST_PROFILE.json`, `WALL_CORNER_REUSE.json`; raw captures and
live-frame subsets under `scratchpad/performance_noassist/`. The existing native
ADF and distribution ZIP are unchanged.

## Span sorting, clipping and line-setup benchmarks

`tools/performance/benchmark_wall_pipeline.py` benchmarks actual production C
compiled at `-m68000 -O2` in Musashi. Two previously captured worlds contain
14/30 records and produce 14/22 merged spans. Wall edges are reconstructed from
those records, the snapshot's cached directions, zoom and pulse, retaining
whole-wall culls and shared-edge suppression: 37/52 line requests. These are
representative snapshot-derived inputs, not a recorded complete frame edge stream
(the snapshot may lie between simulation and render updates; hub/spokes omitted).
Exact fixtures and source hashes are embedded in `WALL_PIPELINE_BENCHMARK.json`.

Cycles below are per workload batch, averaged over ten repetitions with the
empty harness setup subtracted. Span cost includes projection and merging;
clipping cost includes result stores; line-pipeline cost includes clipping,
fill-fix setup and ordinary one-dot setup. Columns are separate workloads and
must not be added together.

| Work / alternative | Hexagon cycles | Hexagonest cycles | Change vs its baseline |
|---|---:|---:|---|
| Current span preparation | 17,195 | 33,535 | baseline |
| Shell-sort spans | 24,695 | 45,593 | 43.6% / 36.0% slower |
| Scan sectors, insertion-sort within each | 18,447 | 32,511 | 7.3% slower / 3.1% faster |
| Current clipping | 33,822 | 46,736 | baseline |
| Early inside accept + left reject | 36,670 | 51,352 | 8.4% / 9.9% slower |
| Current clipping + line setup | 61,947 | 88,197 | baseline |
| Remove redundant line delta bounds check | 60,987 | 86,853 | 1.55% / 1.52% faster |

The final candidate retains all original endpoint range checks. Once both
endpoints are within 320x200, the subsequent checks that horizontal delta is
within +/-319 and vertical delta is at most 199 are implied. Removing just that
second check saves 960 / 1,344 instruction cycles across these batches.

Every candidate matched the baseline output on these workloads: merged span
records, clipping results including right-edge fixes, or custom-register
snapshots after each line request. Register snapshots are collected in separate
verification builds, so their overhead is excluded from the reported setup
cycles. The register block is inert memory: **no DMA execution, raster contention,
blitter busy waits or pixel-output validation** is modelled. This establishes
CPU setup cost and agreement on these inputs, not universal correctness or FPS.

No production renderer edits were applied. Reject Shell sort and the generic
clip fast path on current evidence; the per-sector scan needs broader scene
coverage before adoption. The redundant delta check is the only consistently
positive candidate, but the gain is small. A coordinate cache still has no
measured advantage and has not been added. Native ADF and ZIP unchanged.

Reproduce against the matching snapshot ELF and raw profiles:

```
python3 tools/performance/benchmark_wall_pipeline.py --execram-source /path/to/execram
```

## Timer/record and collision arithmetic follow-up

Two changes were implemented and profiled separately, preserving a before ELF
and ADF for each:

1. `pc_tick_seconds` uses exact uint16 reciprocal `/60` for ticks <=65535 and
   retains the original 32-bit division above that range. Both live timer and
   record formatting derive remainder as `ticks - quotient*60`, avoiding a
   second division. Record formatting skips unchanged displayed values, with
   no separate cache lifetime to invalidate. Full-width quotient is retained
   until remainder calculation, even when display seconds wrap to uint16.
2. `pc_collide` looks up 120/90/72/60-degree sector widths for sides 3..6 and
   uses reciprocal MULU/SWAP for normalised 0..359 angles. Other angles retain
   original signed division; other side counts retain the original divisor
   calculation. Rollback recalculates the sector from the restored angle.
   Three-sided support is preserved without claiming it is normally reached.

Accuracy: `tests/time_conversion_test.c` checks 1,066,003 quotient/remainder
cases (all uint16 values, fallback boundary, one million wider values, display
wrap and UINT32_MAX). Expanded save-boundary tests verify profile changes,
repeated record loads and large saved values. `tests/collision_reciprocal_test.c`
compares the original algorithm across every int16 angle bit pattern, sides
3..6 and four wall configurations: **1,048,576 cases, zero mismatches** in angle,
previous angle, hit and blocked flags. Both tests are wired into `make test`.
The full regression suite, game save/loading checks, and native ADF build's
link/relocation/target/image checks passed. These binaries were also booted
successfully into the gameplay captures.

### Timer/record A/B

99 complete PAL video frames in each existing comparison window, with guest
sound and steering assist active:

| Scene | Before FPS | After FPS | Unsigned helper self time |
|---|---:|---:|---:|
| Hexagon around 90s | 27.23 | 27.73 | 7.14% → 5.79% |
| Hexagonest around 45s | 14.12 | 15.13 | 7.54% → 6.11% |

Observed frame counts rise 54→55 / 28→30. The small windows do not establish
whole-level percentage gains. Evidence: `TIMER_PROFILE.json`.

### Collision A/B

The initial window gained one frame in Hexagon but lost one in Hexagonest,
so two more windows were measured before deciding to retain the change.
Across three separate 99-video-frame windows at absolute emulator times
190, 200 and 210 seconds:

| Scene | Before rendered frames | After rendered frames |
|---|---:|---:|
| Hexagon | 178 | 180 |
| Hexagonest | 96 | 96 |

This is about a 1.1% sampled rendering gain for Hexagon and no aggregate FPS
change for Hexagonest. The initial helper self-time falls 5.79%→5.09% /
6.11%→5.46%; changed render/simulation boundaries affect these percentages.
The additional windows execute no death ticks. Retained for exact arithmetic,
lower division cost and the small positive/non-regressing aggregate result,
not as a promised whole-level speedup. Evidence: `COLLISION_PROFILE.json`.

Raw data: `scratchpad/performance_timers/`, `scratchpad/performance_collision/`.
Audio cue `/200` and the redundant line-setup guard are unchanged in this pass;
they still require their own production validation and FPS comparison.
The native ADF includes the retained changes. `Hexagon.zip` remains unchanged.

## Audio cue-index optimisation

`pc_pulse_cue_index_offset` now checks `sample < lead`, subtracts the lead, and
uses a single 68000 `DIVU.W` when the dividend is below 13,107,200. That strict
bound guarantees the quotient fits in 16 bits (at most 65,535); larger values
retain the original full-width `/200`. No incremental cursor/cache is introduced,
so seeks, loops, reverse playback and lead changes retain the pure function's
original behaviour. This replaces the general helper's two word divides for
normal soundtrack positions. A full-width reciprocal multiply would need more
work on the 68000 and was not assumed to be an improvement.

Actual target-code validation in Musashi passes **599,837 checks**: for each
quotient 0..65,536, test the first/last sample and the preceding sample (when
present), for all three leads 0/582/612; also check pre-lead positions, 10,000
full-width pseudorandom values and extreme inputs. This exercises the target
assembly fast path as well as the overflow-boundary fallback, rather than only
host code. It is boundary coverage plus a mathematical quotient bound, not an
exhaustive enumeration of every uint32 pair. Pulse/cue regression tests pass,
including newly added fallback boundary cases.

Musashi `-m68000 -O2` measurements, including identical loop/result-check overhead
but excluding DMA/IRQs, show **654.08 → 372.08 cycles per call** over 7,060 typical
cue lookups: 282 cycles saved, about 43.1%. Expected output is checked during
the run. Evidence: `CUE_BENCHMARK.json` and `tools/performance/benchmark_cues.py`.

The native ADF rebuild passed linking, relocation, target and boot-image checks,
then both binaries booted into the same scripted gameplay scenarios. Three
99-video-frame windows per variant isolate only this change:

| Scene | Before rendered frames | After rendered frames | Sampled FPS before → after |
|---|---:|---:|---:|
| Hexagon | 180 | 180 | 30.25 → 30.25 |
| Hexagonest | 96 | 97 | 16.14 → 16.30 |

The initial windows' unsigned-helper self time drops 5.09%→4.76% / 5.46%→5.15%.
Across the three windows, individual frame counts can rise or fall by one as
render boundaries shift. The aggregate shows no Hexagon FPS change and about
1% improvement in Hexagonest, just one extra frame in roughly six sampled seconds.
This supports retaining the exact, lower-cost routine, but is not a promise of
noticeable or whole-level FPS improvement. These are assist-enabled emulator
measurements, not hardware timings.

Evidence: `CUE_PROFILE.json`; preserved before ELF/ADF/source and raw profiles
under `scratchpad/performance_cues/`. Native ADF includes the retained change;
`Hexagon.zip` remains unchanged. The remaining redundant line delta guard has
not been removed in this pass.

## Retained line delta-guard removal

Removed only the redundant delta guard in `blit_line_onedot`. The original
endpoint checks remain and reject x>319, y>199 and horizontal lines. For valid
endpoints, their difference necessarily lies within +/-319 horizontally and,
after the existing endpoint swap, 0..199 vertically. No blitter register setup,
right-edge fill correction, clipping or wait behaviour changed.

`tools/performance/check_lineguard.py` compiles both actual old/new routines for
68000 and compares all 128 custom-register words after each of 3,969 endpoint
combinations. The grid includes all octants, horizontal/vertical lines, screen
edges, word boundaries and rejected endpoints (including 65535). **Zero register
mismatches.** Independently checked all 142,400 valid horizontal/vertical endpoint
pairs against the implied bounds. Inert registers do not simulate DMA pixels;
this is setup equivalence, supported by the unchanged drawing instructions.
Evidence: `LINEGUARD_ACCURACY.json`.

The prior pipeline benchmark established CPU savings of 960 / 1,344 cycles per
snapshot workload (about 1.5% of clipping plus setup cost). Actual before/after
ADF profiling now isolates this edit, with all preceding optimisations present
in both builds:

| Scene | Before frames | After frames | Sampled FPS before → after |
|---|---:|---:|---:|
| Hexagon | 180 | 183 | 30.25 → 30.76 |
| Hexagonest | 97 | 98 | 16.30 → 16.47 |

Each aggregate covers three separate 99-video-frame windows, about 5.95 sampled
seconds. This is a measured 1.7% / 1.0% rendering increase in these workloads,
not a hardware or whole-level guarantee. Initial-window `blit_line_onedot`
self time drops 10.99%→10.54% / 8.12%→7.86%, despite rendering as many or more
frames. Inputs, PAL A500 configuration, audio and held-8 assistance match the
prior comparisons; additional windows contain no death ticks.

Full `make test`, native ADF linking/relocation/target/image checks and emulator
boots passed. Retained the change. `LINEGUARD_PROFILE.json` contains the detailed
profiles; before ELF/ADF/source and captures are preserved in
`scratchpad/performance_lineguard/`. Native ADF rebuilt; distribution ZIP unchanged.

## Focused Hexagonest profile (current build)

Profiled six separate 99-video-frame windows spanning approximately 45–95 seconds
of Hexagonest gameplay. Three previous windows were reused only after matching
the current ELF hash; three later windows were freshly captured. Configuration:
PAL stock A500 OCS, 512 KiB Chip + 512 KiB Slow, no JIT, guest audio enabled,
host audio muted, held-8 steering assistance. No sampled window executes death
logic. Total sampled time is about 11.9 seconds, not the full intervening minute.

| Approximate gameplay time | FPS | Blitter busy |
|---|---:|---:|
| 45s | 15.13 | 12.2% |
| 55s | 21.68 | 16.1% |
| 65s | 12.61 | 11.0% |
| 75s | 17.65 | 13.4% |
| 85s | 13.61 | 11.9% |
| 95s | 19.67 | 15.0% |

Aggregate sampled rendering is **16.72 FPS**, with **13.28% blitter busy time**.
CPU and blitter overlap; these are not exclusive shares of a frame budget.

| CPU function self time | Aggregate |
|---|---:|
| Steering assistance | 10.16% |
| render_scene (includes inlined wall work) | 9.94% |
| Span projection/sorting/merging | 7.64% |
| One-dot line setup | 7.55% |
| Line clipping | 6.69% |
| Direction-to-coordinate calculation | 5.32% |
| Unsigned division helper | 5.06% |
| Clipped-line wrapper | 4.25% |
| World movement | 3.73% |
| PCM copying | 3.27% |
| Line-mode setup/wait | 2.92% |
| Player angle update | 2.80% |
| Collision | 2.56% |

A separate assist-off capture dies partway through. Excluding the first frame
executing `pc_death_tick` and all subsequent frames leaves 50 complete live frames,
about one second, at **21.96 FPS**. Do not compare this shorter window directly
with the six-window aggregate as a measured assist speedup. Its main costs are
render_scene 12.50%, line setup 11.93%, clipping 10.06%, spans 8.30%, clipped-line
wrapper 6.61%, coordinate conversion 6.34%, and unsigned division 3.69%.
No `cheat_steer` samples remain in the filtered window.

Conclusion: Hexagonest remains CPU-bound in these captures. The next substantial
candidate is reducing repeated work across the clipping/wrapper/line-setup path
(e.g. an internal prepared-line path), with exact fill parity and register/pixel
checks. Wall preparation remains significant, but previously tested Shell sort
and generic clip fast paths were slower, so those should not be adopted. Audio
and the remaining generic arithmetic are smaller targets. Assist cost must stay
explicit in future FPS comparisons.

Evidence: `HEXAGONEST_PROFILE.json`; raw snapshots and new profiles under
`scratchpad/hexagonest_profile/`. Two initial late profiles ran out of disk space
and are excluded; their successful `_complete` retries are used. Losslessly
compressed 5,300 older generated `slots-*.bin` traces with gzip to recover space;
use `gunzip` on the relevant `.bin.gz` when a tool needs raw bus slots. JSON
summaries, CPU samples, source snapshots and ELF/ADF baselines were preserved.
No production code, ADF or ZIP changes in this profiling pass.

## Prepared clipped-line path

Extracted line setup into an always-inlined private `blit_line_prepared` worker.
The public `blit_line_onedot` retains its endpoint/horizontal-line checks. The
clipped wrapper invokes the worker directly when `RenderClip.line` is true,
using the clipper's guarantee that the endpoints are on-screen and non-horizontal.
This removes the duplicate checks and public-function call on the hot path.
Y ordering, seed-span tracking, right-edge fill corrections, register writes and
blitter waits retain their original logic. The linked native binary contains
`blit_clipped_line_onedot` with inlined setup; the unused public entry is removed
by normal section garbage collection.

Accuracy: actual old/new 68000 code produces identical custom-register blocks
for **11,664 clipped-line cases**, covering all octants, word/screen boundaries,
horizontal/vertical, signed off-screen inputs up to +/-8191 and right-edge fixes.
An additional **3,969 public-entry cases** preserve rejected-input behaviour.
Both report zero mismatches. These tests use inert custom-register memory and
verify setup, not hardware DMA pixels; existing clip/projection regression tests
also pass. Evidence: `PREPARED_LINE_ACCURACY.json`,
`PREPARED_PUBLIC_ACCURACY.json`, `tools/performance/check_prepared_lines.py`.

Hexagonest before/after uses six separate 99-video-frame windows around 45–95
seconds, PAL A500 OCS, 512 KiB Chip + 512 KiB Slow, no JIT, guest audio and
steering assist enabled. Later before windows are reused only after an exact
ELF hash match. Every sampled window remains in gameplay.

| Metric | Before | After |
|---|---:|---:|
| Rendered frames over ~11.9 sampled seconds | 199 | 204 |
| Aggregate sampled FPS | 16.72 | 17.14 |
| Combined line-setup + wrapper CPU self time | 11.81% | 10.65% |

This is a **2.5% sampled rendering improvement**. Compare combined self time,
not individual symbol percentages, because line setup moved into the wrapper.
Per-window rendered counts are 30→31, 43→44, 25→25, 35→36, 27→28, 39→40.
The gain is consistent across these workloads but is not a whole-level or
hardware FPS guarantee. Evidence: `PREPARED_LINE_PROFILE.json`.

Full `make test`, native ADF linking/relocation/target/image checks and emulator
boots passed. Before ELF/ADF/source and raw captures are preserved under
`scratchpad/performance_prepared/`. The native ADF includes this change;
`Hexagon.zip` remains unchanged.

## Reciprocal division in span preparation

Added an exact multiply/shift path to `project_div5` for nonnegative uint16
values: `MULU #52429`, word swap and a two-bit shift. Ordinary wall distances
and widths use this range. Negative or wider inputs still use the previous
bounded signed DIVS.W path or general signed division. Sorting, merging,
wall visibility and outward-ending radius construction are unchanged.

Actual 68000 validation exercises public span projection against independent
signed division for every distance -65,540..65,540 with varying widths, INT32
extremes, and reciprocal/hardware fallback boundaries. All checks pass; the
host regression range extends to +/-163,850. Complete merged spans also match
byte-for-byte on the two captured-world benchmark workloads. `make test` passes.
Evidence: `SPAN_MATH_ACCURACY.json`, expanded `tests/projection_checks.c`.

| Whole span-preparation workload | Before cycles | After cycles | Reduction |
|---|---:|---:|---:|
| Hexagon captured world | 17,195 | 14,199 | 17.4% |
| Hexagonest captured world | 33,535 | 28,827 | 14.0% |

These are Musashi 68000 `-O2` instruction counts, without DMA/IRQs, including
projection, sorting and merging, not merely the divide operation. Workload and
source hashes are retained in `SPAN_MATH_BENCHMARK.json`; reproduce with
`tools/performance/benchmark_span_math.py --execram-source /path/to/execram`.

Six matched-current-build Hexagonest windows around 45–95 seconds show:

| Metric | Before | After |
|---|---:|---:|
| Rendered frames over ~11.9 sampled seconds | 204 | 208 |
| Sampled FPS | 17.14 | 17.48 |
| Span-preparation CPU self time | 7.88% | 7.01% |
| Span-preparation CCK per rendered frame | 16,295 | 14,230 |

That is a **2.0% sampled rendering gain**. Per-window frame counts are
31→32, 44→45, 25→26, 36→36, 28→28, 40→41. Both builds use PAL stock A500 OCS,
512 KiB Chip + 512 KiB Slow, guest sound and steering assist enabled. Later
baseline profiles were reused only after matching their ELF hashes. No sampled
death ticks; this is not a continuous whole-level or hardware FPS guarantee.

Native ADF link/relocation/target/image checks and gameplay boots pass. Retained
the change; native ADF rebuilt, distribution ZIP unchanged. Raw data and before
ELF/ADF/source are under `scratchpad/performance_spanmath/`; full profiling
results are in `SPAN_MATH_PROFILE.json`. Compressed earlier prepared-line bus
slot files losslessly to `.bin.gz` to keep space available.

## Reuse hub vertices for spokes

The solid spoke overlay now reuses the hub vertices already projected earlier
in the same frame. This removes one repeated radius conversion and 3–6 repeated
coordinate projections per frame, using 24 bytes of static storage. Every scene
refreshes the vertices before the overlay; no reuse occurs across scenes.

`tools/performance/check_hub_reuse.py` compares the extracted old and new
production routines: **131,072 scenes, identical hub/spoke line commands**,
including culling, 3–6 sides, changing angles, zoom, pulse and camera offsets.
This host comparison substitutes deterministic trig for the assembly routines;
it verifies reuse semantics, not DMA pixels. Full `make test` passes.

Six 99-frame Hexagonest PAL A500 OCS emulator windows, around gameplay 45–95s,
show **208 → 211 rendered frames**, or **17.48 → 17.73 FPS (+1.44%)** across
11.90 sampled seconds. Per-window counts: 32→32, 45→45, 26→26, 36→37, 28→29,
41→42. Guest audio and steering assist remain enabled; no death ticks occurred.
Baseline profiles match the saved before ELF hash; the first three baseline
windows were also rerun. This small sampled gain is not a whole-level or
hardware FPS guarantee.

Native build checks and gameplay boots passed. Native ADF rebuilt; distribution
ZIP unchanged. Evidence: `HUB_REUSE_ACCURACY.json`, `HUB_REUSE_PROFILE.json`,
and `scratchpad/performance_hub/` (before source, ELF/ADF, captures and profiles).

## Repeat benchmark and current priorities

Fresh cold boot of the current hub-reuse build, followed by the same six
Hexagonest windows, reproduces **all six profile summaries exactly**, including
ELF hash, instruction self-time totals, bus counters and render counts. Counts
are 32, 45, 26, 37, 29, 42: **211 renders / 11.89894 seconds = 17.73267 FPS**.
This is deterministic repeatability, not a second independent statistical sample.
No death ticks occurred. Settings remain PAL A500 OCS, 512K Chip + 512K Slow,
no Fast/JIT, guest sound and steering assistance enabled. No game code changed.

| Current function/path | Aggregate self time |
|---|---:|
| Clipped-line wrapper, including inlined prepared line setup | 11.00% |
| render_scene, including inlined wall projection/culling | 10.60% |
| Steering assistance | 10.02% |
| Line clipping | 7.15% |
| Span projection/sorting/merging | 7.09% |
| Direction-to-coordinate calculations | 5.33% |
| Unsigned division helper | 5.08% |
| World movement | 3.75% |
| PCM copying | 3.28% |
| Line-mode setup/wait | 3.14% |
| Shared-edge detection | 2.27% |

Blitter busy time is **14.08%**, bus ownership **6.90%**, and CPU stalls charged
to blitter-nasty **1.58%** (all CPU bus waits total approximately 3.29%). CPU and
DMA overlap; these percentages must not be added as exclusive budgets. Idle bus
slots do not imply an idle CPU. The evidence still favours CPU preparation work.

Next-pass priorities, ranked by a combination of likely benefit and bounded risk:

1. **Benchmark exact fixed-zoom radius scaling.** All six profile-entry Chip RAM
   snapshots contain `zoom == 128`. `zscale` currently multiplies an unsigned
   16-bit radius by zoom then shifts eight bits; at 128 it can use an unsigned
   shift by one. Keep the general path for camera transitions/endings. Test all
   65,536 radius bit patterns, inspect target assembly, and measure the whole
   wall path: a per-call branch may consume the saving, so consider a scene-level
   selection only if it wins. Do not combine zoom with direction multiplication
   by changing the intermediate truncation. This is a candidate, not a measured
   improvement; the 10.60% render_scene cost is not all radius scaling.
2. **Clipping/line setup remains the largest substantial target: 18.14%.**
   Investigate call/struct handoff and repeated endpoint ordering in the actual
   target assembly, or a specialised radial-edge path. Preserve clipping order,
   intersection truncation, right-edge fill toggles and half-open line endpoints.
   Require exact register/command comparisons plus rendered-pixel checks before
   acceptance. The earlier generic clip fast paths were slower; do not repeat
   that change without new evidence.
3. **Span/shared-edge preparation: 9.35% combined.** Benchmark tighter record
   traversal and bookkeeping on captured worlds, preserving record order and
   exact merged output. Prior Shell sort and per-sector sorting results do not
   justify replacement. Generic coordinate caching stays below these candidates
   because measured Hexagonest corner reuse was low.
4. **Attribute remaining arithmetic before replacing it.** The 5.08% unsigned
   helper is shared across callers; do not credit it all to gameplay rendering.
   Steering prediction performs variable-speed divisions and repeated turns.
   Optimising assistance can improve test-mode FPS without improving normal play;
   keep that work and its reported benefit separate.

PCM copying and fill overlap are lower priorities than the above. For each
accepted change use identical six-window captures and target correctness checks;
add a deterministic input-replay survival benchmark without assistance before
claiming a normal-play FPS uplift. Continuous whole-level and hardware timing
remain unmeasured here.

Evidence: `HEXAGONEST_REPEAT_PROFILE.json`; fresh capture/profile scripts, states
and logs under `scratchpad/performance_repeat/`. Bus-slot dumps were losslessly
gzipped after summarising; prior hub-profile slot dumps were also compressed to
keep disk space available. Existing ADF and distribution ZIP were not rebuilt.

## Fixed-zoom radius scaling: retained

`zscale` now uses an unsigned shift at zoom 128; all other zooms retain the
existing multiply-and-shift. Casting the radius to unsigned before the shift
preserves the old behaviour for negative/wrapped radius bit patterns. Direction
projection and its intermediate rounding are unchanged. Production disassembly
shares one comparison between the two wall radii and emits two `lsr.w #1`
instructions on the gameplay path.

`tools/performance/benchmark_zoom.py` executed the extracted old/new routines on
Musashi 68000: **589,824 exact matches** (all 65,536 radius bit patterns at zooms
0,64,127,128,129,256,320,512,65535). The unchanged fallback covers other zooms.
The normal-zoom loop falls from 112.03 to 59.05 cycles per iteration (-47.3%);
these figures include loop/store overhead and allow invariant-check hoisting,
so they are not isolated production call costs. Zoom127/320 loop results are
essentially unchanged; they do not prove zero fallback overhead in the game.

Six matched-baseline Hexagonest windows produce:

| Metric | Before | After |
|---|---:|---:|
| Rendered frames / 11.89894 sampled seconds | 211 | 213 |
| Sampled FPS | 17.73 | 17.90 |
| render_scene CPU self time | 10.60% | 9.99% |
| render_scene CCK / rendered frame | 21,198 | 19,794 |

This is **0.95% higher sampled FPS** and approximately **6.6% less render_scene
self time per rendered frame**. Window counts: 32→32,45→45,26→28,37→37,29→29,
42→42. These short windows quantise rendering progress; most show no additional
completed render. Settings remain assisted PAL A500 OCS with guest sound,
512K Chip + 512K Slow; no sampled death ticks. No whole-level/hardware uplift
is claimed. Baseline ELF hash was checked against the saved before executable.

Full `make test`, native build/package checks and gameplay boots passed. Retain
the change. Native ADF rebuilt; distribution ZIP unchanged. Reports:
`ZOOM_BENCHMARK.json`, `ZOOM_PROFILE.json`; sources, disassembly, before ELF,
captures and raw profiles under `scratchpad/performance_zoom/`.
Next substantial target remains the clipping/line-setup path identified above.

## Line row-offset arithmetic: small CPU saving, no measured FPS gain

The prepared-line path now computes its 40-byte row stride as `(5*y)<<3` using
explicit word shifts/addition. Clipping guarantees Y0..199, so the result fits
16 bits. Other screen strides retain the multiply. An initial ordinary C
expression still compiled to MULS and produced no aggregate rendering gain;
the explicit sequence is retained for its small verified CPU saving.

Accuracy: **11,664 actual 68000 old/new custom-register comparisons**, zero
mismatches, spanning all octants, clipping boundaries and fill fixups. An
exhaustive host check of **64,000 on-screen coordinates** confirms identical
bitmap offsets. Register checks use inert memory; these are not DMA/pixel
equivalence tests. Only address arithmetic changed; clipping order, registers
and drawing operations are preserved. Full `make test` passed, as did native
build checks and fresh gameplay boots.

A complete clipped-wrapper microbenchmark across both endpoint orders and all
starting scanlines saves **10 cycles per line**, 1648.77→1638.77 cycles including
identical loop overhead (-0.61%). This is Musashi instruction execution without
DMA/IRQs, not the representative gameplay edge mix. Reproduce accuracy with
`tools/performance/check_row_offset.py --execram-source /path/to/execram`, then
`tools/performance/benchmark_row_offset.py` for timing.

Six assisted PAL A500 OCS Hexagonest windows show **213→213 renders**, or
**17.90075→17.90075 FPS**. Per-window counts: 32→32,45→45,28→27,37→38,29→29,
42→42. Line-wrapper self CCK per render falls 21,847→21,736 (-0.51%); aggregate
self time falls 11.026%→10.970%. The baseline ELF hash matches the saved before
executable, guest audio remains enabled, and there are no sampled death ticks.
**No frame-rate improvement is demonstrated.** Retain as a small exact CPU
optimisation; pursuing more stride micro-tuning is lower priority than removing
clipping handoff/ordering work or reducing actual edge preparation.

Reports: `ROW_OFFSET_ACCURACY.json`, `ROW_OFFSET_BENCHMARK.json`,
`ROW_OFFSET_PROFILE.json`. Trial and final profiles are under
`scratchpad/performance_row/` and `scratchpad/performance_row_shift/`.
Native ADF rebuilt; distribution ZIP unchanged. Older raw CPU sample files
were losslessly compressed with gzip to keep disk space available.

## Inline clipping into line setup: retained

The hot blitter wrapper now uses an always-inlined clipping implementation,
shared in `render_clip_impl.h` with the existing public `render_clip_line` entry.
The algorithm, clipping order and intersection arithmetic are unchanged. Giving
GCC both routines in one translation unit removes the clipping call and permits
elimination of temporary-result transfers. Production disassembly confirms no
out-of-line clipping call in the wrapper. The public helper remains available
for tests and other callers without maintaining a second algorithm.

Accuracy uses separately compiled saved original clipper and blitter sources:
**11,664 actual 68000 cases match**, including **every byte write to the custom
registers in order**, rather than only final register values. The grid includes
all octants, clipping near-misses, right-edge fill fixups, horizontal/vertical
lines, word boundaries and extreme projected endpoints. Registers are inert
memory in this harness: this verifies drawing commands, not DMA timing or pixels.
Full `make test`, native build checks and fresh gameplay boots passed.

A 68000 complete-wrapper microbenchmark over both endpoint orders and all
starting scanlines falls **1638.77→1254.77 cycles (-23.4%)**, including identical
loop overhead, without DMA/IRQs. This is an on-screen line workload, not the
full gameplay mix. The actual six-window Hexagonest comparison is:

| Metric | Before | After |
|---|---:|---:|
| Render calls / 11.89894 sampled seconds | 213 | 233 |
| Sampled FPS | 17.90 | 19.58 |
| Clipping + line-wrapper CPU self time | 18.17% | 14.68% |
| Combined CCK per rendered frame | 36,006 | 26,592 |
| Blitter busy time | 14.20% | 15.52% |

**9.39% higher sampled FPS**, with **26.15% lower combined clipping/setup cost
per rendered frame**. Counts are 32→36,45→50,27→30,38→41,29→31,42→45.
Combine both function names for comparison: clipping self time moves into the
wrapper after inlining; its disappearance as a separate symbol is not itself
a speedup. Baseline hash matches the saved before ELF. Settings remain PAL A500
OCS, 512K Chip + 512K Slow, guest audio and steering assistance enabled. No
sampled death ticks. These are short emulator windows, not a whole-level or
hardware guarantee. More frames naturally increase blitter utilisation.

Retain the change. Native ADF rebuilt; distribution ZIP unchanged. Evidence:
`CLIP_INLINE_ACCURACY.json`, `CLIP_INLINE_BENCHMARK.json`,
`CLIP_INLINE_PROFILE.json`, and `scratchpad/performance_clipinline/`.
Accuracy reproduction: `tools/performance/check_clip_inline.py --execram-source
/path/to/execram`. Timing: `tools/performance/benchmark_clip_inline.py` (uses
that check's before objects and the prior row-offset check's timing probe).
Next reassessment should use this new baseline; wall projection and span/shared
edge preparation remain candidates, rather than assuming the old clipping
percentages still apply.

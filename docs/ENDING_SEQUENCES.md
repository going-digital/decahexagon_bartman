# Ending sequences

Latest preview: F8 runs the secret ending with Focus played backwards at its
normal pitch. The player is hidden throughout the animation and completion
screen. Successful completion stops the music, triggers WONDERFUL once, and
shows the centred overlay CONGRATULATIONS / GAME COMPLETE. Retry/menu exit
clears the completion state. Overflow recovery does not announce success.
User playtest feedback (2026-09-25): the ending animation looks really good.
This confirms the visual presentation; it is not a full hardware compatibility sweep.

Stage-2 bonus play holds field rotation at zero. Its soundtrack is the separate
512-sample stretched Focus arrangement, using two Paula channels. Test entry
refreshes the selected profile's record without writing test scores.
Full PC ending parity is not claimed.

## Verified entry behaviour

`tools/capture_pc_ending.py` checks the owned PC executable SHA-256, maps a private
copy and runs original x86-64 code. Filesystem, music, palette and rendering
side effects are intercepted; the installed PC game is not modified.
The checked output is `tests/fixtures/pc_ending_entry_native.txt`.

`gameclass::winlevel` at `0x100009b50` produces these first-completion states:

| Selected profile | Completion state |
| --- | --- |
| Hexagon / Hexagoner | 1 |
| Hexagonest | 2 |
| Hyper Hexagon / Hyper Hexagoner | 1 |
| Hyper Hexagonest | 3 |

Already-completed profiles do not set a new completion state. All 12 combinations
were executed against the PC routine. `make test-ending-completion` checks
the current Amiga completion classification against these fixtures, including
the strict 3,600/3,601-tick boundary; all cases pass. Unlock congratulations (state 1), normal
completion (state 2), and secret completion (state 3) must remain distinct.

`gameclass::secretending` at `0x100029a90` sets stage 4, clears the wall scene,
restores six sides, resets extent/timing and sequence state, requests music ID 5
and effect ID 3, selects palette 30, clears graphics buffers and fully detaches
the camera. The probe verifies these entry fields and intercepted calls.
The three gameplay soundtrack banks are therefore not sufficient by themselves
to claim secret-ending audio support. Map music ID 5 to its actual PC asset and
measure its disk/memory cost before choosing storage or compression.

## Timeline evidence awaiting a native trace

The existing `decomp_gamelogic_100056f50.c` export calls `secretending` when
completion state 3 reaches death/result extent greater than 199, after the
60-tick death hold. Stage 4 uses a separate millisecond timeline and renderer.
The decompile shows sequence phases including thresholds greater than 24,499,
35,499 and 46,499 ms; after 167,999 ms it returns to stage 2 with completion
state 2. These are inspection findings, not yet independently executed boundary
traces. The input export also contains explicit secret-ending entry paths;
replay/unlock access needs tracing rather than assuming first completion only.

## Implementation order and acceptance

1. Extend the native probe to normal-result entry, secret timeline boundaries,
   input/skip/replay, and exit state. Record audio, palette and wave requests.
2. Map ending music and cues from the owned PC assets; measure ADF free space
   and decoded bank capacity. Preserve existing gameplay bank cache ownership.
3. Add a portable ending state machine checked against the native fixtures;
   keep ordinary retry/death fixtures passing. Ending time must not inflate
   records or trigger saves during the sequence.
4. Connect explicit ending modes, camera/geometry, result text and audio to the
   renderer. Specify any Amiga visual adaptations rather than silently claiming
   perspective parity.
5. Exercise first completion, replay, skip/back, return to selection and saves
   on PAL/NTSC, native ADF and WHDLoad. Build/package only after integration.

The current release remains unchanged by this investigation. Achievements also
need actual award logic: the current save field is persisted but never awarded.

## Courtesy-derived ending audio investigation (2026-09-24)

The user's proposed sources are slowed Courtesy and reversed Courtesy. The owned
PC files decode as music4.dat = 77.7405 s and music5.dat = 162.4295 s. The PC
play routine separately uses nominal loop lengths of 77,000 and 162,000 ms;
decoded file duration must not silently replace that scheduling behaviour.

`tools/audio/compare_ending_courtesy.py` compares both files with PC music1.dat,
forward and reversed, at playback speeds 0.5, 2/3, 0.75, 0.8, 1 and 1.25.
For each candidate it searches the full transformed source for eight-second
windows at ending times 5, 30 and 60 seconds, using normalized waveform
correlation. The best mean absolute correlations are approximately 0.079 and
0.071, respectively. These tests do **not** establish an exact reconstruction;
they also do not disprove a shared source with edits, effects, independent
pitch/time stretching, or a different mix. See `ENDING_AUDIO_COMPARISON.json`
for inputs, hashes, rates and per-window matches.

The local audition page at `scratchpad/audio/ending_courtesy/index.html` includes
both full PC ending files and forward, half-speed and reversed versions of the
accepted preboosted/Fibonacci Courtesy bank. These transforms have no deliberate
ending-specific cuts or fades and are explicitly labelled hypotheses. No
additional audio is included in the distributed ZIP and runtime playback has
not changed. Next: establish a convincing musical/time mapping before choosing
Paula period changes, reverse buffer filling or an offline reconstruction.

## Non-audio phase controller implemented

`pc_ending.c` / `pc_ending.h` now implement the secret-ending phase controller:
entry acceleration, timed phases 0–15 and 101, palette/flip requests, rotation
and camera targets, hyper/scene flags, music-stop notification and final exit.
The audio notification is an event only; no ending audio code has been added.
The nominal 60 Hz clock is independent of score and record updates.

`make test-ending-timeline test-ending-completion` passes. Phase tests cover
threshold-minus-one/exact-threshold cases, five-tick holds, flip acknowledgement,
palette blending gates, one-shot stop events and a complete 10,080-tick
run with an open exit gate. Exit timing was subsequently corrected as described
below. Completion classification still matches the original native entry fixture.
These timeline tests validate a transcription of the decompile, not an
independently executed native timeline oracle.

This module is deliberately not connected to the release game yet: the stage-4
wall selector, spawn-delay/finish ordering, camera geometry and normal completion
presentation still need implementation and reference comparison. It must not
replace ending walls with ordinary stage patterns. No new release ZIP was built
for this isolated controller milestone. Audio reconstruction remains deferred
at the user's request.

## Ending wall selector implemented and native-checked

`pc_ending_waves.c` now selects the stage-4 wall families from ending time,
frozen score, polygon size/shrink state and speed. It preserves the PC's RNG
bounds, thresholds and unused draw. The existing generator already contains
wave 1001 used by the final large geometry; no substitute pattern was added.

`tools/capture_pc_ending_waves.py` runs the original full gamelogic routine with
phase progression held at phase 15 and wave generation intercepted. The probe
captures selected wave and random-draw count, along with the actual selector
inputs after the PC update. This isolates selection from phase effects and
avoids making a full-timeline equivalence claim.

`make test-ending-waves` matches all 27,200 native cases: 17 times around selector
boundaries, five score inputs, five polygon sizes, two shrink states, two speeds,
and 16 seeds. Source table 0x1001bbad0 was checked directly: its RNG bounds are
5.0 and 6.0. An initial inverted mapping for polygon sizes 3–5 was caught by the
native comparison and corrected. See `ENDING_WAVES_RESULTS.json`.

Release integration remains outstanding: spawn-delay/marker ordering, camera
and final-scene rendering, normal result presentation, entry/skip/replay, and
record/save isolation. Audio remains deferred. The phase controller, completion
classification and selector tests all pass; the distribution is unchanged.

## Spawn/exit gate and generator adapter implemented

A new original-PC probe, `tools/capture_pc_ending_gate.py`, covers 56 combinations
of ending time, positive/zero delay and marker state. `make test-ending-gate`
checks the portable gate against these captures. A marker holds a positive
delay; zero delay can still spawn with a marker present. At 163,500 ms the PC
stops spawning and renews a 30-tick delay whenever the gate opens. At or after
168,000 ms it exits only when that gate opens. Thus the previously documented
unconditional 168-second phase-controller exit was incorrect and is now removed.

`pc_ending_spawn` connects the phase/exit gate, ending selector and existing
geometry generator. Adapter tests exercise a first spawn, countdown suppression
without RNG consumption, final wave 1001 generation without overflow, and final
exit without a further draw. Completion, phase, all 27,200 selector cases and the
56 native gate cases pass together.

The game-facing integration is still pending. `pc_ending_step` produces phase
requests; the caller must handle camera/palette/rotation effects in reference
order, move the world, and dispatch spawning at the correct point in its update.
No release gameplay mode or camera has been replaced with a placeholder. Audio
remains deferred, and the release ZIP is unchanged.

## Ending palette transitions implemented and native-checked

Added the missing palette 200: the PC uses its default yellow-to-red endpoints
for this ID. Added explicit ending palette entry and transition requests to
`pc_palette`, retaining the existing two-colour OCS projection. Ending callers
must tick with score zero, then apply phase palette requests; this begins the
fade on the following tick without ordinary gameplay score milestones.

`tools/capture_pc_ending_palette.py` checks the owned executable SHA-256 before
running its original setpal and gamelogic routines. Phase progression is held
at phase 15 and spawning suppressed; ten palette requests are injected at
100-tick intervals. The fixture therefore verifies palette blending under
controlled requests, not the natural ending timeline or rendered scene.
`tests/fixtures/pc_ending_palette_native.txt` records palette 200 endpoints and
1,000 ticks of scheme, transition state, blend, direction and RGB slots 0/2.

`make test-ending-palette test-palette test-ending-gate test-ending-timeline
 test-ending-waves test-ending-completion` passes: all 1,000 ending palette
ticks and the existing 21,900 ordinary palette ticks match the PC references,
as do the earlier ending gate, selector and completion cases.

Camera/final-scene rendering, normal results, entry/skip/replay, runtime update
ordering and record isolation remain pending. These palette APIs are not yet
called by a live ending mode. Audio remains deferred; no release ZIP was built.

## Ending camera controls implemented (integral 60 Hz state)

`pc_ending_camera.c` implements seekangle, nullotisrotate and otisrotate at
nominal dt=1 for integral degree state. Seek and null adjustments approach a
target by one degree. Otis adjustment is one-sided: even modes decrease toward
-10 (mode 0) or -20 (2/4/6/8); odd modes increase toward +10 (1) or +20
(3/5/7/9). Values already beyond that limit are left unchanged. Unknown modes
are unchanged. This is not a symmetric easing function.

`tools/capture_pc_ending_camera.py` hash-checks the same owned binary, then
executes the original routines at 0x10000aa10, 0x10000aa70 and 0x10000ab30.
The fixture tests initial angles -60..60, rotation modes 0..10, null adjustment,
and seek targets 0/10/20/30. All 1,936 cases match via `make test-ending-camera`.
The existing ending palette, timeline, gate, selector and completion tests pass.

Scope is explicitly integral state and dt=1. Fractional PC camera state and
variable-dt behavior are not represented by these helpers. Before integration,
verify inherited entry angles and preserve phase-specific call ordering: an
unchanged camera target field does not mean seekangle is called every frame.
Camera projection, scene transforms and game-facing ending modes remain
outstanding. Audio and the release ZIP are unchanged.

## Camera phase dispatch

`pc_ending_camera_tick` now dispatches camera adjustments from the incoming
ending phase and wave mode. Call it before the phase controller. The five-tick
acceleration holds suppress camera calls; phases 5, 10, 101, 14 and 15 make no
camera adjustment. In particular, retaining a previous target does not keep
seeking during those phases. Transition ticks still use the old wave mode.
The API accepts inherited camera angles rather than silently resetting them.

All 256 phase values are checked with and without a hold, against dispatch
transcribed from gamelogic. These are source-derived dispatch tests, not a
native full-timeline comparison. The 1,936 native primitive cases and timeline
regression still pass. Full scene transformation/projection and live gameplay
integration remain pending; the release ZIP and audio are unchanged.

## Native scene projection reference

`tools/capture_pc_ending_projection.py` executes the original transformscene
routine at 0x10000ab90 in the hash-checked owned binary. It supplies host libm
sine/cosine tables and one vertex, stage 4, no extra rotation/zoom animation,
centre (384,240), and focal scales (600,600). The 360-case fixture covers tilt
0/10/20/30, Otis -20/-10/0/10/20, depth 0/300/600 and six 3D vertices.

The recovered order is rotation around X by wrapped -tilt, rotation around Y
by wrapped Otis, then depth translation by base_depth - 5*tilt. Negative depth
is reflected positive, and exactly zero becomes 0.1 before perspective division.
That behavior differs from conventional near-plane clipping. Integer screen
coordinates truncate after adding the screen centre, including negative values.

`make test-ending-projection-reference` verifies the host equations against all
360 native captures, with 1e-8 coordinate tolerance and one pixel tolerance for
host trigonometric/fixture rounding. This is an executable reference, not an
Amiga renderer implementation or a visual end-to-end test. Animated offsets,
non-stage-4 transforms and final-scene vertex construction are not covered.
The next implementation must choose fixed-point precision and safe clipping
without silently replacing these depth semantics. Audio remains deferred and
the release ZIP is unchanged.

## Fixed-point projection implementation

`pc_ending_projection.c` now implements the stage-4 transform using an offline
Q20 sine table and 64-bit integer intermediates, with no runtime floating point.
It preserves rotation order, depth reflection and the zero-depth fallback.
The API bounds coordinates/depth to +/-8192, tilt to +/-90 and focal scales to
4096. Screen results saturate to +/-32767; this is an arithmetic guard, not
polygon clipping. Invalid arguments leave output coordinates unchanged.

`make test-ending-projection` compares 351 native fixture cases at depth >=1:
maximum measured pixel error is zero. Nine singular-plane cases are explicitly
excluded from native pixel equivalence; zero-depth centre projection, saturation
and invalid-input behavior have separate checks. This is not evidence of exact
agreement for every vertex or camera configuration. The module compiles for
68000, but 64-bit arithmetic cost has not been profiled. It is not linked into
release gameplay yet; polygon clipping, vertex construction and integration
remain outstanding. Audio and the release ZIP are unchanged.

## Projected polygon clipping

`pc_ending_clip.c` clips convex projected quads to inclusive viewport bounds,
producing up to eight vertices. Full signed-16-bit input coordinates are allowed;
intersection products use 64-bit intermediates to avoid overflow. Duplicate
vertices and zero-area results are discarded, and input/output may alias.
This handles polygons enclosing the whole viewport, which cannot be rejected
simply because their original vertices are all outside the screen.

`make test-ending-clip test-ending-projection` passes 2,025 rectangle cases,
eight-vertex clipping in both winding directions, and the existing projection
reference comparisons. The clipper also compiles for 68000. It remains isolated
from live gameplay and has not been performance-tested on Amiga.

The input contract is convex quads. Projector saturation can distort edges near
the singular plane; clipping saturated points does not establish equivalence to
clipping the PC's original wide coordinates. That integration issue, final-scene
vertex construction, draw submission and ending lifecycle remain outstanding.
Audio stays deferred; this milestone does not rebuild the distribution.

## Unsaturated projection-to-clipping handoff

Added `pc_ending_project_wide` and widened clipper points to signed32. The new
path retains projected coordinates up to +/-1,000,000,000 instead of flattening
them to signed16 before clipping. Both interfaces reject values outside that
bound, keeping 64-bit intersection products safe. Callers must reject a polygon
when any projection fails; partial output must not be submitted. The original
saturating projection wrapper remains available but is not the clipping path.

Tests now pass projected zero-depth quads directly into clipping, verifying that
coordinates beyond signed16 produce the full viewport boundary. Billion-unit
clipping bounds and out-of-contract rejection are also covered. Existing 2,025
rectangle cases and 351 non-singular native projection comparisons still pass;
both modules compile for 68000. The nine native singular-plane cases remain
excluded from pixel-equivalence claims. No live rendering or performance claim
is made; scene construction, draw submission and ending lifecycle remain open.
Audio stays deferred and the release ZIP is unchanged.

## Quad preparation adapter

`pc_ending_polygon` now projects all four vertices, validates projected convexity
and clips before publishing output. It returns a visible vertex count, zero for
invisible/degenerate geometry, or -1 for invalid projection/nonconvex geometry.
Failed or invisible preparation leaves the caller's output unchanged. The
convexity check also rejects crossed vertex ordering; the convex clipper must
not receive quads folded by depth reflection.

`make test-ending-polygon test-ending-clip test-ending-projection` passes visible,
viewport-enclosing, invalid-last-vertex, crossed and degenerate quad cases plus
the existing projection/clipping checks. The adapter compiles for 68000.
Rejecting folded quads is a safety boundary, not PC visual equivalence: their
rendering remains unresolved. Scene vertex generation, draw submission, runtime
integration and hardware profiling remain pending. Audio remains deferred and
no new distribution was built for this milestone.

## Blitter edge submission

`render_ending_quad` now joins quad preparation to the existing
`blit_clipped_line_onedot` primitive. It submits a closed loop only after all
projection, convexity and viewport clipping checks succeed, using the same XOR
fill-seed convention as ordinary playfield polygons. Invalid/invisible quads
issue no blitter commands. The frame caller must clear its back buffer, reset
fill bounds, establish line mode, perform the final fill and swap buffers.

The new translation unit compiles with the native 68000 flags and warnings as
errors. Polygon, clipping and projection regressions pass. No rendered-frame
or emulator comparison has yet been performed; this is an isolated draw adapter,
not live ending integration. Multiple overlapping polygons share XOR parity,
so scene construction must account for this rather than assuming an opaque
polygon painter. Folded geometry, scene construction, frame lifecycle and
performance remain unresolved. Audio and the release ZIP are unchanged.

## 68000 performance gate — current projection is reference-only

The object compilation checks above did not establish linkability: the SDK has
no libgcc and project support lacks __muldi3/__divdi3. The current projection
therefore needs extra arithmetic support even before integration. Do not ship
this implementation as a real-time renderer.

`python3 tools/benchmark_ending_projection.py --execram-source /path/to/execram`
builds a runnable baseline with explicitly benchmark-only shift/add multiply
and restoring divide helpers, then measures Musashi 68000 instruction cycles.
At 7 MHz: 4 vertices take 60.75 ms, 40 take 557.50 ms, 100 take 1370.36 ms,
and 200 take 2716.07 ms. Empty-loop overhead is 412 cycles. Exact results are in
`ENDING_PROJECTION_BENCHMARK.json`. No DMA contention, clipping, scene updates
or drawing is included. This is not an optimized-libgcc benchmark or measured
hardware timing, and the coordinate workload is synthetic, not a captured scene.

The next implementation milestone is a bounded 16/32-bit projection with camera
coefficients computed once per frame and shared vertices reused. Benchmark that
against the same workload before further scene integration. Preserve this slow
path as a correctness reference. Audio remains deferred; release ZIP unchanged.

## Planar affine camera candidate

`pc_ending_affine` replaces per-vertex depth with centre depth for z=0 geometry.
A Q12 matrix is prepared once per frame from tilt/Otis angles and focal scales;
points use three signed16 products and power-of-two scaling. Nonpositive centre
depth and coefficients outside signed16 are rejected. This is an approximation,
not a substitute for arbitrary nonplanar scene geometry.

`make test-ending-affine` checks 3,380 points against the analytic affine
transform (within 1.5 pixels). Native Musashi benchmark with `--affine` reports
2.72 ms for four vertices, 15.94 ms for 40, and 37.94 ms for 100 at 7 MHz.
Setup/empty-loop cost is 1.23 ms. These include synthetic coordinate-generation
and function-call overhead; they exclude clipping, drawing, DMA and gameplay.
See ENDING_AFFINE_BENCHMARK.json. The four-point slow reference took 60.75 ms.

This is cheaper but not yet a proven frame budget. Reuse shared vertices and
cache unchanged camera matrices before increasing workloads. Visual comparison
against PC perspective and live integration remain pending. Audio stays deferred;
release ZIP unchanged.

## Affine benchmark overhead and matrix cache

Inspection of the 68000 output confirms three native muls.w instructions per
point; no general multiplication helper is called by the point transform.
The initial benchmark's per-point modulo-based coordinate generator was costly.
The new `--incremental` benchmark generates the same sequence with additions and
conditional subtraction (for the measured 0..200 vertices). It measures 1.71 ms
for four vertices, 5.88 ms for 40, 12.81 ms for 100 and 24.34 ms for 200 at 7 MHz.
Setup/empty-loop cost remains 1.23 ms. These figures supersede the earlier
modulo-workload figures for estimating transform cost; this is a harness
improvement, not a corresponding speedup in the point transform itself.
Exact results: ENDING_AFFINE_INCREMENTAL_BENCHMARK.json. DMA, clipping, drawing
and gameplay remain excluded.

`pc_ending_affine_cached` now reuses a matrix when all view parameters match.
Invalid views invalidate the cache and cannot expose stale coefficients. Tests
cover reuse, changed centre, invalid depth and recovery alongside the existing
3,380 affine cases. Cache hits have not yet been timed on target; the timings
above still include a fresh setup. The cache must be zero-initialized by its
caller. Live scene integration and visual comparison remain outstanding; audio
and the release ZIP are unchanged.

## Affine fill-seed submission path

`pc_ending_affine_quad` prepares all four planar vertices atomically, rejecting
nonzero Z and coordinates beyond the existing edge clipper's +/-8191 contract.
`render_ending_affine_quad` submits those edges directly to the established
blit_clipped_line_onedot path. It intentionally retains offscreen edges so
right-boundary XOR parity fixes still handle viewport-enclosing polygons.
This path avoids the perspective projector and 64-bit polygon clipper entirely.
The caller supplies an ordered planar quad and prepares/caches its matrix once
per frame; buffer clearing, line mode, fill and display handoff remain caller-owned.

Affine tests pass, including invalid-last-vertex, nonplanar input and oversized
projection rejection without publishing partial output. The drawing adapter
compiles for 68000. Combined drawing cost and emulator visuals are not measured;
live scene generation and lifecycle integration remain pending. Audio remains
deferred and the distribution is unchanged.

## Planar wall construction

Added `pc_ending_wall_quad` and `render_ending_affine_wall`: a merged radial
span now becomes a planar quad and can pass through affine projection to the
blitter edge adapter. Geometry uses the existing Amiga wall convention: first
inner, next inner, next outer, first outer, with radial pulse applied equally.
The caller supplies field-rotated, unscaled Q14 Cartesian directions, reusable
for every span in the frame. Do not pass the ordinary renderer's camera-scaled
direction table or raw overlapping wall records. Inputs outside the planar
projection budget are rejected without partial output.

Wall/pulse/bounds/affine-handoff tests pass, as do existing affine checks. Both
wall construction and drawing adapter compile for 68000. This reuses the
Amiga planar wall representation; it is not native-verified equivalence to the
PC compose3dframe_ending routine, which includes additional scene branches.
Final-scene geometry, live frame dispatch, visual comparison and whole-frame
performance remain pending. Audio and the release ZIP are unchanged.

## Planar world wall pass

`pc_ending_scene_walls` and `render_ending_affine_walls` now process a complete
world's planar walls: merge overlapping same-slot spans using the existing
render-only union, reuse one cached matrix and caller-provided sector directions,
construct quads and submit their edges. Simulation records remain unchanged.
The reusable scene workspace must be zero-initialized and allocated outside
the small frame stack. Rejected spans are counted instead of silently claiming
complete rendering. The pass supports 3..6 sides; other states return failure.

Scene tests verify overlap merging, immutable world state, invalid-camera
rejection and rejected-span accounting. Scene and blitter adapters compile for
68000. This is a wall pass, not a complete ending frame: hub/background/player,
final-scene geometry, lifecycle dispatch, visual comparison and aggregate timing
remain pending. Audio and release ZIP are unchanged.

## Affine hub perimeter

Added `pc_ending_scene_hub` and `render_ending_affine_hub`. They prepare and
submit a single 3..6-sided perimeter using the same matrix and sector directions
as the walls. Radius includes the caller's pulse. A closed perimeter preserves
the existing XOR fill convention without introducing triangle-fan diagonals.
All vertices are validated before output or blitter submission; zero/invalid
radius, invalid side count/directions and unsafe projected coordinates reject
without partial drawing.

Scene tests include known hub coordinates and atomic rejection of an invalid
last direction, alongside the wall-pass checks. Scene and drawing modules
compile for 68000. Player/background, final-scene geometry, live lifecycle and
visual/performance validation remain pending. This is the agreed planar camera
approximation, not a new claim of PC scene equivalence. Audio and ZIP unchanged.

## Affine player marker

Added player preparation using three caller-provided rotated directions and
nose/base radii, transformed with the same affine matrix as the walls/hub.
It reuses player_shape_build and rejects invalid/oversized geometry with an
empty shape. The drawing adapter waits for the blitter, then overlays the solid
marker with viewport-clipped CPU writes, after the frame fill. The frame owner
must hide ordinary player sprites to avoid a duplicate marker.

Scene tests cover a valid player and invalid final direction clearing the shape.
Both modules compile for 68000. This adapts the existing Amiga marker rather
than claiming PC scene-geometry parity. Frame integration, final scenes and
visual/whole-frame timing remain pending. Audio and release ZIP unchanged.

## Complete affine drawing-path link audit

`make test-ending-affine-target` now links the actual scene, affine geometry,
player shape, renderer, edge clipper and blitter sources into a freestanding
68000 executable. Required wall/hub/player drawing, clear, line-mode and fill
entry points are retained explicitly so linker garbage collection cannot mask
missing dependencies. It uses the project's existing assembly support only.
The audit rejects unresolved symbols and inclusion of 64-bit multiply/divide,
perspective projection or the wide polygon clipper.

The link passes: text is 5,738 bytes and read-only data 182 bytes (includes shared
blitter/geometry routines; not the incremental release size). Scene scratch and
frame buffers are caller-owned and are not included in that figure. Existing
scene and affine tests pass. See ENDING_AFFINE_TARGET_RESULTS.json.
This executable has not run against custom-chip emulation; entry is a link-audit
stub, not a bootable game. Frame execution/timing, lifecycle and final-scene
integration remain pending. Audio and release ZIP unchanged.

## Interactive affine/perspective comparison

Run `python3 tools/preview_ending_affine.py` to generate
`scratchpad/ending_visual/index.html`. It compares actual C affine and reference
perspective outputs for 108 synthetic planar scenes: tilt 0/20/30, Otis -20/0/20,
and twelve field rotations. Controls select the same frame in both views.
The untilted/unrotated-camera cases differ by at most one pixel. Strong tilt
visibly needs review; no visual acceptance is claimed from numeric tests.

This is a host Canvas even-odd-fill preview, not a captured PC ending, blitter
emulation or performance proof. It deliberately labels synthetic geometry and
omits audio, player/sprites, palette timing and final-scene construction. It
provides a concrete approximation review before live scene integration.
Release ZIP unchanged.

## Accepted camera simplification: tilt 0, Otis 0

User selected a flat ending camera. `pc_ending_camera_tick` now holds both
angles at zero in every phase, and the presentation matrix cache independently
normalizes both angles before lookup. Ordinary in-plane field rotation remains
in the caller-provided sector/player directions. The old PC camera dispatch is
retained under pc_ending_camera_reference_tick for reference tests only.

Zero-angle matrix preparation skips trigonometry and shear construction; point
projection skips the shear multiply. The scene uses scale and translation, with
no perspective or camera tilt. Direct affine prepare remains general for the
comparison tool. Tests verify phase holds stay flat and changing requested tilt
or Otis cannot alter the cached presentation matrix. Camera, affine and scene
checks pass; the complete drawing path still passes its 68000 link audit.
Updated performance has not been measured. Live integration and final-scene
construction remain pending; audio and release ZIP are unchanged.

## Flat production setup and measurement

The presentation cache now calls a dedicated scale/translation setup, keeping
the general affine setup and sine table out of the linked renderer. Equal focal
scales share one division. The target link audit now rejects those reference
symbols as well as the perspective/64-bit helpers. Linked text is 5,196 bytes
with no read-only trigonometry table; this includes shared drawing routines,
not just incremental game code.

The `--flat --incremental` benchmark exercises the production cache on a cold
setup: 0.30 ms setup/empty loop, 0.75 ms for four vertices, 4.59 ms for 40,
10.97 ms for 100 and 21.58 ms for 200 at 7 MHz. Exact results are in
ENDING_FLAT_BENCHMARK.json. This includes coordinate generation/call overhead,
but excludes wall construction, clipping, drawing, DMA and gameplay; no whole
frame rate is claimed. Affine and scene tests plus the 68000 link audit pass.
Live ending integration, final scenes and visual validation remain pending.
Audio and ZIP unchanged.

## Consolidation onto the normal renderer

With the accepted flat camera, `render_scene` now takes an explicit RenderScene
snapshot and optional PcWorld. The ordinary render_game wrapper supplies exactly
its existing gameplay fields and wall-visibility decision. Hub/walls, shared-edge
suppression, spokes and player atlas/overlay all consume the same frame snapshot.
Endings can supply their world and scene without mutating gameplay globals or
using the separate experimental affine drawing stack. The experimental modules
remain reference work, not a second production renderer to integrate.

The native game links without OS symbols/unresolved imports. Player geometry,
atlas tests (65,536 angles), and 8,000 world shared-edge cases pass. Updated the
player test harness for explicit scene state and existing chip-allocation APIs;
also repaired a stale display-handoff extraction boundary at an OS-only #if.
The display-handoff regression passes. Emulator visuals are not yet checked.
Ending mode/lifecycle dispatch and final presentation remain pending. No release
ZIP was rebuilt; audio remains deferred.

## Explicit death-to-secret-ending gate

`pc_ending_death_entry` identifies the transition after death timer/extent update:
completion state 3, death timer >=60, expanded extent >199 and no result
suppression. This transcribes the branch immediately before secretending in
gamelogic (superhex offsets 0x194, 0x19c, 0x5764 and 0x5755). It does not treat
completion states 1/2 as secret entry and does not wait for retry extent 320.
Boundary tests cover each completion state, timer 59/60, extent 199/200 and
suppression. Timeline, native completion classifications and exit-gate tests pass.
This helper is not yet invoked by the live death path: full stage-4 update
ordering, flip acknowledgement and exit/result-state setup still need wiring.
The boundary tests are source-derived, not native execution of the death branch.
Audio and release ZIP remain unchanged.

## In-plane flip control

`pc_ending_flip` implements the updatevisualeffects rotation burst independently
of camera tilt/Otis. A request selects direction on the next effects tick:
modes 1/3/5/7 positive, others negative. Fourteen acceleration ticks add half
of the timer in degrees; 31 hold ticks add seven degrees; ten deceleration ticks
add half the descending timer. Including selection this lasts 56 ticks and
turns 292 degrees. It replaces ordinary field rotation while active, rather
than adding to it. Results use half-degree integer units; the game-facing
caller must retain that precision when converting to render angles.

Source-derived tests cover 11 mode values, every trajectory tick and repeat
requests; timeline regression passes. These are not native flip traces, and the
live game is not yet calling the module. The controller can use phase!=0 as its
flip acknowledgement once update ordering is connected. Tilt/Otis stay zero.
Audio and release ZIP unchanged.

## First live secret-ending integration (2026-09-25)

MODE_ENDING is now entered from the first Hyper Hexagonest completion's death
expansion gate. Entry preserves the completed run's score, resets walls/morph,
selects palette 30 and stops gameplay audio. The update loop runs the independent
ending clock, flip controller, palette requests, morph/wall movement and ending
wave selector. It does not call record_time or advance the gameplay lifecycle
clock. The normal renderer draws the ending world, hub, spokes and player;
tilt/Otis and 3D camera requests remain disabled. Random camera-effect requests
still consume their draw without changing the wall mode. Escape uses the normal
menu path, making accumulated progress eligible for saving.

At ending completion the current implementation clears the world and enters the
existing retry/results state with completion state 2. This is an interim result
presentation, not the PC's complete final sequence. Scene-mode 1/2 choreography,
normal-ending presentation, replay access, audio and visual parity are unfinished.
Overflow also returns to results rather than rendering unsafe wall storage.

Native linking and component tests (phase/flip, 27,200 wave cases, 56 exit-gate
cases, completion classifications and player geometry/atlas) pass. These do not
establish end-to-end runtime correctness: emulator/hardware entry, full duration,
Escape/retry/save isolation and timing still require validation. No distribution
ZIP has been rebuilt with these unfinished endings.
## Ending preview camera fix

The F8 preview inherited the title zoom because MODE_ENDING skipped the entire
ambient update. Camera easing now runs in ending mode while gameplay cue/pulse
processing remains disabled. `tests/ending_camera_easing_test.py` exercises the
production update in both zoom directions and checks ordinary pulse behaviour.
That regression, the ending timeline check and the ADF/WHDLoad distribution
build pass. `out/Hexagon.zip` includes the fix. Hardware confirmation is pending;
this fixes the identified camera freeze, not proof that every reported visual
stall has the same cause. The opening field angle still settles and holds until
the first flip at approximately 24.5 seconds, as specified by the controller.

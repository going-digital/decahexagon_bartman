# PC accuracy implementation progress

Updated 2026-09-19. See [the implementation plan](PC_ACCURACY_PLAN.md).

Current milestone: PC-unit walls, collision, exact waves and the normal-Hexagon
selector/morphs are connected to live gameplay. All 132,039 saved generator and
Hexagon/Hexagoner selector reference cases pass. Presentation and stage progression are
still incomplete; see the third batch below.

## First batch completed (historical)

- Explicit Amiga source lists prevent host test programs from entering the target link.
- Added the portable `pc_core` and shared host/68000 checks, plus a preserved original-machine-code mechanics trace with provenance.
- Live gameplay now accumulates integer PC degrees at 7 degrees per nominal tick for the current normal-Hexagon mode. Removed gameplay flick/snap state. The old renderer's opposite handedness is handled at the adapter boundary.
- Input retains both direction bits; left/PC input43 takes priority. Direction signs are adapted to the existing rendering coordinates.
- Simulation runs at nominal60 Hz on PAL and NTSC, independent of completed presentation count. A PAL five-frame interval executes six ticks. Missed presentations retain elapsed time; fire/back edges are consumed only once.
- Pixel-aspect selection remains tied to the display standard, not the now-common simulation frequency.
- Added PC-unit collision and ordinary wall-motion functions with boundary checks against the original routine traces. These functions are **not yet connected to live pixel-unit walls**.
- Fixed an emulator-discovered NTSC deadlock: `WaitVbl` previously waited for PAL raster line311. It now waits for beam wrap on both standards. Also made the main-loop frame-counter comparison unsigned across the signed-counter boundary.
- Added repeatable build, emulator configuration and screenshot/input scripts. Private FS-UAE instances do not change the user's Launcher configuration.

## Verification performed

| Check | Result |
| --- | --- |
| Host C checks with warnings treated as errors | Pass |
| Host UndefinedBehaviorSanitizer checks | Pass |
| Original collision trace boundaries: 6 distances ×3 widths | Pass in host and target self-test |
| Tick conversion, turn rates/wrap/priority, ordered collision restoration and ordinary shrink | Pass in host and target self-test |
| FS-UAE PAL A500, 512 KB Chip +512 KB Slow | Target PASS; title and gameplay observed |
| FS-UAE NTSC A500, 512 KB Chip +512 KB Slow | Initially reproduced startup deadlock; after fix, target PASS and title/play/game-over observed |
| FS-UAE PAL A500, 512 KB Chip only | Release-flag target PASS; title/play/game-over observed |
| Final normal PAL release build | Unpacked and packed executables/ADFs built; packing self-check passed; normal unpacked ADF smoke-tested |
| Whitespace/diff check | Pass |

The emulator configurations use an owned Kickstart1.3 ROM, stock68000 and no Fast RAM/JIT. Core tests execute inside the target program; screenshots of PASS are not host-only test results.

AddressSanitizer could not initialize on this host: process sampling showed its initialization recursively entering its own allocator and spinning before main. Both stalled test processes were stopped. This is not counted as a passing ASan run; the ordinary and UBSan runs completed.

Representative local evidence (ignored scratchpad artifacts):

- [PAL target PASS during play](scratchpad/fsuae/pal/fs-uae-crop-2609190954-01.png).
- [512 KB target PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609190959-01.png).
- [512 KB game over](scratchpad/fsuae/pal512/fs-uae-crop-2609191000-02.png).
- [NTSC target PASS after the fix](scratchpad/fsuae/ntsc/fs-uae-crop-2609191001-01.png).
- [NTSC game over](scratchpad/fsuae/ntsc/fs-uae-crop-2609191001-03.png).
- Configurations/logs/screenshots under `scratchpad/fsuae`; [artifact manifest](scratchpad/fsuae/verification_manifest.json).

Build/test instructions are in [tests/README.md](tests/README.md). Use a forced rebuild when changing flags, since configurations currently share object paths.

## First-batch size measurements (superseded)

First-batch PAL release, before live generator integration:

| Item | Bytes |
| --- | ---: |
| Code `.text` | 15,664 |
| Read-only data | 1,212 |
| Initialized ordinary data | 42 |
| BSS | 2,872 |
| Embedded Chip audio data | 198,055 |
| Static Chip section | 8 |
| Three dynamically allocated bitplanes | 24,000 |
| Dynamic copper list | 1,024 |
| HUD sprite payload allocations (88 ×40) | 3,520 |
| Unpacked executable on disk | 221,464 |
| Packed executable on disk | 143,908 |

These section/payload counts are not a complete peak-memory budget: include allocation overhead, hunk alignment, loader/decrunch workspace, OS and stack. The packed image's boot-time peak has not been emulator-tested in this batch. No future six-mode/audio memory guarantee follows from the present 512 KB smoke test.

## Remaining work and next batch

Phase0/1 are **partial**, not fully signed off. The portable interface, nominal clock and selected numeric semantics are implemented; a complete reference-tick oracle, calibrated hardware-time policy, full float/fixed-point equivalence and worst-case raster/stack measurements remain.

Next: verify full ordered normal-mode tick traces and complete stage progression,
then implement the other five selectors and their transition behavior. Replace
the temporary planar camera, cue/pulse and death/menu presentation, and measure
worst-case raster/stack use. The third batch below connects the new simulation
to live gameplay, but does not establish overall PC accuracy.

The nominal clock currently uses configured50/60 Hz, not a measured fractional refresh rate. PAL input is sampled once per displayed frame, so its occasional second simulation tick uses the same held state. Long-frame behavior targets nominal PC60 Hz gameplay, not the desktop's variable-delta clamp. No worst-case performance guarantee or subjective audio/input-latency claim was established by these smoke checks.

## Second batch: PC-unit store and exact generator (historical)

- Added a 500-record PC-unit store with ordered first-free-slot reuse, ordinary
  movement/shrink, marker20/21 morph requests and marker22 camera requests.
  Capacity exhaustion is explicit rather than silently overwriting walls.
- Implemented all 45 verified wave IDs using shared immutable decision/emission
  tables (24,870 bytes before compiler layout). Wave405 remains unimplemented,
  matching the audited absence of geometry for that selector request.
- Promoted compressed branch/native fixtures into the repository and added
  reproducible table/target-case generation scripts.
- `make test`: all 9,159 original-machine-code generator cases pass at
  speeds22/33/40, including ordered records and RNG draw counts. Host shared
  checks also pass under UndefinedBehaviorSanitizer.
- Added one original-reference case for each of 45 IDs to the actual 68000
  self-test, plus marker, capacity and slot-reuse checks.

At this milestone the ordinary-integer generator was isolated; live integration
was completed in the third batch below. Marker effects are requests; interpolation
and camera response are not implemented here. The target interpreter contains
no floating-point arithmetic or dynamic allocation. Its runtime RNG callback
must be side-effect-free with respect to the wall store. Nonempty-store source
traces, fractional-delta delay behavior and unknown-ID calls with a nonzero
existing delay still require reference coverage before broader claims.

Target verification: release-flag self-test build passed in FS-UAE PAL,
stock 68000, 512 KB Chip RAM and no Slow/Fast RAM. The executable was253,580
bytes including test data/code. [Captured target PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191022-03.png).
The earlier size table describes the normal live build, where unused generator
code/data is stripped; it is not an integrated-generator memory measurement.

## Third batch: live normal-Hexagon integration

- Replaced approximate timed pattern rows with the verified PC generator and
  normal-Hexagon selector. PC spawn distances, widths and wave-event speed
  changes now drive live gameplay. Removed speed-scaled thickness/spawn
  geometry, accelerated tail shrink and biased gap placement.
- Connected ordered PC collision before wall movement. Restored angles from
  blocked turns are reflected in rendering; newly generated walls first move
  on the following simulation tick.
- Added marker-driven shrink/growth with the desktop binary64 endpoint timing
  at dt=1 (11 shrink ticks; growth initialization plus 11 decay ticks). The
  render arc follows the two distinct repeated 0.1 trajectories. Full
  interrupted/fractional morph equivalence remains outside this model.
- Added a temporary 2D projection adapter: PC radius 40+distance/5, separately
  truncated width/5, opposite-handed angular conversion, and closure of the
  final morph sector back to the field origin.
- Merge overlapping radial spans only in a render buffer to avoid XOR holes;
  the 500 simulation records retain source ordering and independent lifetimes.
- Kept existing title/ready/death/game-over flow and music-driven zoom. Normal
  selector rotation draws are correct, but their planar response is a
  placeholder; tilt/camera requests and rotation cues are not yet reproduced.
- Normal-Hexagon selection matches all 61,440 saved native cases. The shared
  target suite now includes 86 selector cases, morph endpoint timing, marker
  waits and render-span checks, plus a live first-wave travel invariant.
- Host stress: 256 forced-survival runs through tick 10800 reached 48 records at
  most and 6,826 uninterrupted morph requests. Sides 4..6 were reached naturally
  in those samples. These are safety checks, not native full-run comparisons.

Scope: integer-speed, nominal 60 Hz normal-Hexagon mechanics. Stage changes at
180 seconds, hyper transitions, other modes, full death timing, camera/3D
projection, cue-driven pulse, native soundtrack and worst-case performance
remain unfinished. The game currently continues the normal selector after
180 seconds; that continuation is explicitly not desktop-accurate.

The expanded simulation suite passed on PAL 512 KB and NTSC 1 MB. Gameplay
captures then exposed a pre-existing renderer arithmetic defect: `muluw`
declared its output operand as 16-bit, truncating the 32-bit result. PC wall
radii overflowed this limit and wrapped distant walls onto the screen. Fixed
both multiply helpers to use 32-bit output operands and added 68000 regression
checks for 700×128=89,600 and the signed counterpart. Earlier 10:33–10:38
captures are diagnostic evidence before this correction, not final renderer
acceptance screenshots.

Final normal PAL release section sizes after integration (without test overlay):

| Item | Bytes |
| --- | ---: |
| Code `.text` | 18,074 |
| Read-only data | 25,162 |
| Initialized ordinary data | 8 |
| BSS | 10,644 |
| Embedded Chip audio | 198,055 |
| Static Chip section | 8 |
| Unpacked executable | 246,564 |
| Packed executable | 150,200 |

Bitplane/copper/HUD allocation payloads remain 24,000/1,024/3,520 bytes. These
are section/payload counts, not a measured peak or stack budget. Packed
self-decompression passed the host packer's byte comparison; packed boot-time
memory is not yet measured.

A further PAL smoke test exposed display-memory corruption from conditional
viewport clipping. A host reproducer found input (722,-369)→(-1115,568) could
reach the blitter as (0,0)→(722,-369). Added a portable clipper that clips Y
unconditionally before X, preserves right-edge fill parity and guarantees
on-screen endpoints. The raw blitter now validates absolute endpoints before
seed tracking/address calculation. One million host segments and the UBSan
suite pass; 512 large-coordinate cases run in the target suite.

Post-fix PAL 512 KB verification: target PASS (including full-width multiply,
512 clip cases and live first-wave timing), gameplay, game-over and retry all
observed without the corruption above. [Corrected PAL gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191049-01.png),
[game-over](scratchpad/fsuae/pal512/fs-uae-crop-2609191049-02.png),
[retry game-over](scratchpad/fsuae/pal512/fs-uae-crop-2609191049-04.png).

Post-fix NTSC A500 512 KB Chip +512 KB Slow: target PASS, gameplay and game-over
observed. [Corrected NTSC gameplay](scratchpad/fsuae/ntsc/fs-uae-crop-2609191051-01.png)
and [game-over](scratchpad/fsuae/ntsc/fs-uae-crop-2609191051-02.png).
The final normal/packed PAL executable and ADF builds succeeded. Build hashes
and capture paths are recorded in
[scratchpad/fsuae/integration_manifest.json](scratchpad/fsuae/integration_manifest.json).

Final uninstrumented PAL release also booted and reached gameplay/game-over
on the 512 KB Chip-only A500. [Release gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191053-01.png)
and [release game-over](scratchpad/fsuae/pal512/fs-uae-crop-2609191053-02.png).
All verification emulator instances were closed; `out/hexagon.adf` and
`out/hexagon_packed.adf` contain the normal PAL release.

## Fourth batch: Hexagoner selector and playable build

The previous verified milestone was committed as `e416109`.

- Added the normal Hexagoner selector: wave 91 opener, wave 1 special pool,
  forced-fourth and forced-twentieth waves, deep pools, and score-dependent
  RNG bounds. Preserved the six-way branch that deliberately emits no wave.
- Implemented its event-driven speeds independently of Hexagon: 24 initially,
  ramp toward 28 only at waves 12–29, and post-3600 ramp toward 33. Unlike Hexagon,
  this selector does not freeze its delay for a morph marker wait.
- Added `PC_START_STAGE=1` for a playable Hexagoner build with the correct
  title and a live first-wave travel assertion. The default stays Hexagon.
- Promoted 61,440 stage 1 native selector traces. Combined selector comparison
  now passes 122,880 cases; the 9,159 wave-generator comparisons still pass.
- Expanded shared host/68000 selection checks from 86 to 274 native cases.
- Added 256 Hexagoner forced-survival stress runs to tick 10800: peak 61 records,
  no overflow and no morph requests. Host checks and UBSan pass.

The 180-second stage change is not a simple difficulty increase: the reference
clears several state groups and enters another stage with hyper-entry state.
This batch provides the next selector that path needs, but does not yet add
that handoff, unlocks, rank announcements or the six-level menu. Hexagonest,
hyper-entry/ending behavior and complete ordered tick traces remain next.

Hexagoner PAL A500 512 KB target checks passed; title, gameplay and game-over
observed. [HEXAGONER target PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191105-01.png),
[live timing PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191106-01.png),
[game-over](scratchpad/fsuae/pal512/fs-uae-crop-2609191106-02.png).

Hexagoner NTSC A500 (512 KB Chip + 512 KB Slow) target checks and live timing
passed. [NTSC live PASS](scratchpad/fsuae/ntsc/fs-uae-crop-2609191111-01.png)
and [walls in gameplay](scratchpad/fsuae/ntsc/fs-uae-crop-2609191111-02.png).

Separate PAL release artifacts are available as `out/hexagoner.adf` and
`out/hexagoner_packed.adf`; executable sizes are 248,528 and 150,780 bytes.
The default Hexagon PAL release was rebuilt as `out/hexagon.adf` and
`out/hexagon_packed.adf` (248,828 and 150,900 executable bytes).
Both packing self-checks passed; packed disks have not been boot-tested.
The emulator runner accepts `FSUAE_ADF` to test a separate disk without
replacing the default build.

The uninstrumented Hexagoner PAL release booted and reached gameplay on the
512 KB Chip-only A500. [Release title](scratchpad/fsuae/pal512/fs-uae-crop-2609191112-03.png),
[release gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191112-04.png),
[later gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191112-05.png).
Verification emulator instances were closed after the captures.

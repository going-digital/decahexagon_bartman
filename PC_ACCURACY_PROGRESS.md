# PC accuracy implementation progress

Updated 2026-09-19. See [the implementation plan](PC_ACCURACY_PLAN.md).

Current milestone: PC-unit walls, collision, exact waves and all three normal
base-level selectors are connected to playable builds. All 193,479 saved
wave-generator and selector reference cases pass. Presentation and stage
progression remain incomplete; see the fifth batch below.

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

## Fifth batch: Hexagonest selector and playable build

- Added the normal Hexagonest selector, including wave92 opener, forced waves,
  nested split pools and the score-dependent 3/4/7-entry deep pool. The owned
  Mach-O constants at `0x1001bbac8` independently confirm bounds3/4.
- Added 61,440 native stage2 traces: all 184,320 selector cases and 9,159 wave
  cases pass. Shared host/68000 coverage now contains 402 native selector cases.
- Stage2 wall speed is 35 below wave12 and becomes 40 at scheduling points
  after score3600. Player input uses 9 degrees per tick rather than 7.
- Preserved stage2 rotation-mode RNG order (modes2/3 at wave5, then4/5,
  6/7 or8/9). Extended the temporary planar adapter to cover every mode safely.
- Added the one-time post-7200 clear/rotation event before delay countdown:
  it consumes its RNG draw even when no wave is due and preserves speed/delay.
  Native `clearenemies` disassembly at `0x100029d40` confirms record clearing
  without resetting the scheduler. The selector traces exposed this extra draw.
- Preserved flip requests at waves12/24/48 and every fifth wave after score8000.
  Their visual effect is still pending, as are palette transitions, music/cue
  changes and the remaining camera state of the post-7200 event. The event
  currently assumes a settled palette; full graphics transition gating is absent.
- Added `PC_START_STAGE=2` and a HEXAGONEST title. The first80-tick live assertion
  checks speed35 and first-wall travel from4050. Hexagon remains the default.
- All 768 forced-survival runs through tick10800 pass: peak records48/61/61
  for stages0/1/2, with no stage1/2 morph requests or store overflow. These are
  stress tests, not full desktop replays. Shared checks also pass under UBSan.

Hyper-entry wave95, stage handoffs, unlocks, rank announcements, ending behavior
and the six-level menu remain unfinished. This milestone implements the normal
selector, not complete Hexagonest progression or presentation.

PAL A500 512 KB Chip-only target checks passed during gameplay and game over:
[PAL live PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191129-02.png),
[PAL game-over PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191129-03.png).
The initial title capture exposed clipping of the longer HEXAGONEST banner;
letter spacing now contracts to fit the existing 128-dot sprite canvas.

The corrected full title and target PASS were observed on NTSC A500 (512 KB
Chip + 512 KB Slow): [NTSC title PASS](scratchpad/fsuae/ntsc/fs-uae-crop-2609191130-01.png).
The initial NTSC gameplay captures lost the diagnostic overlay. Added an
explicit blitter wait before CPU overlay writes to avoid concurrent access;
this is self-test-only and does not affect release gameplay.

The uninstrumented Hexagonest PAL release reached gameplay and game over on
the 512 KB Chip-only A500:
[release gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191132-01.png),
[release game over](scratchpad/fsuae/pal512/fs-uae-crop-2609191132-02.png).
Separate artifacts: `out/hexagonest.adf` and `out/hexagonest_packed.adf`, with
250,928-byte normal and 151,600-byte packed executables. Both disks built and
the packing self-check passed; the packed disk has not been boot-tested.

After synchronizing overlay writes, NTSC live timing checks remained PASS
through the death flash and game over:
[NTSC live PASS](scratchpad/fsuae/ntsc/fs-uae-crop-2609191134-01.png),
[NTSC game-over PASS](scratchpad/fsuae/ntsc/fs-uae-crop-2609191134-02.png).
Verification instances were closed. The default PAL Hexagon normal/packed
releases were rebuilt (250,868/151,684 executable bytes), with packing
self-check passing; `out/hexagon.adf` remains the default Hexagon release.

## Optional development steering assist

`CHEAT_MODE=1` adds a held-top-row-8 steering aid. It chooses ordinary left/right
input using a 48-tick look-ahead over existing records. Collisions, turn rates,
RNG and wall geometry are unchanged. Releasing 8 restores normal input. This is
not invulnerability or a guarantee against future spawns/morph changes.

`CHEAT_MODE=0` is the default, including debug builds. The disabled source list
omits `cheat.c`; preprocessor guards remove the call, input field, raw-key
handler and held-key state. A generated configuration header invalidates target
objects whenever the option changes, preventing stale cheat-enabled objects
without requiring a clean build. Every disabled link audits its ELF symbols
and map for cheat functions/data/object inclusion. The audit was also tested
against the enabled binary and correctly rejected its `k_cheat` symbol.

Host assist tests pass for left/right escape routes, look-ahead without mutation,
command exclusion and an unavoidable lethal ring (normal collision remains).
The full PC reference suite and stress tests pass. The route search caches sector indices and skips searching when remaining
still is safe. Initial long captures were misread as a performance/display
problem; a disabled-build control showed the normal post-death score blink.

The final planner computes wall danger intervals and future blocked-sector
lookups instead of repeatedly simulating every wall. It replans at10 Hz while
steering at60 Hz, and invalidates its cached route on release, restart, side
count or speed changes. `tools/build.sh release` explicitly forces
`CHEAT_MODE=0` and runs the binary audit, even when the parent make command
requests `CHEAT_MODE=1`.

The original AppleScript `key down "8"` automation did not sustain the input.
A direct macOS key-event test reached19.35 seconds on the 512 KB A500 and died
shortly after releasing8, matching the intended ordinary-collision behavior.
[Post-release result](scratchpad/fsuae/pal512/fs-uae-crop-2609191204-01.png).
`tools/fsuae_assist.swift` now provides a reproducible host-only hold/release
smoke test. Cheat builds show a small8 while the key is held; that indicator is
also completely excluded from release builds.

The final host assist suite includes96 seeded normal-game simulations with
real collision (32 per base level), each capped at60 seconds. Hexagon minimum
survival was22.67 seconds and mean48.76; Hexagoner/Hexagonest all reached the
60-second cap. This is regression coverage, not a guarantee of perfect play.
Release builds were switched directly from enabled to disabled without `-B`,
and `CHEAT_MODE=1 release` correctly forced the option off. Normal/packed
release artifacts were rebuilt and the no-cheat audit passed.

Final FS-UAE 512 KB Chip-only evidence:
[assisted play at10.41 seconds with held8 indicator](scratchpad/fsuae/pal512/fs-uae-crop-2609191206-01.png),
[death at21.10 after release, indicator absent](scratchpad/fsuae/pal512/fs-uae-crop-2609191207-01.png).
The verification instance was closed. `out/hexagon_cheat.adf` is the separate
development disk; `out/hexagon.adf` and `out/hexagon_packed.adf` are audited
cheat-free release builds. Unit checks also pass under UBSan.

## Floppy motor shutdown during takeover

System takeover now sends synchronous `TD_MOTOR` requests with length0 to all
available `trackdisk.device` units before `Forbid()`. The OS driver turns off
its latched motor state while task switching and interrupts still work, and
retains consistent bookkeeping for disk access after exit. The reply port is
built using Kickstart1.3-compatible Exec calls; absent drives are skipped and
opened devices/signals are released before takeover. No disk data is written.

Normal and packed release builds pass, including the no-cheat audit. FS-UAE
PAL A500 512 KB Chip-only boots and runs the game, then returns to the AmigaDOS
prompt via Escape. [Game with drive-status display](scratchpad/fsuae/pal512/fs-uae-full-2609191216-01.png),
[restored AmigaDOS](scratchpad/fsuae/pal512/fs-uae-full-2609191217-01.png).

A subsequent reload from AmigaDOS exposed an existing exit/restart failure:
the updated binary raised Guru8000000B; a separate baseline using the exact
pre-change `system.c` from `edc7079` also raised a Guru (80000004) when reloaded.
[Updated reload failure](scratchpad/fsuae/pal512/fs-uae-full-2609191219-01.png),
[baseline reload failure](scratchpad/fsuae/pal512/fs-uae-full-2609191225-01.png).
This is not counted as a passing restart test and remains a separate cleanup
issue. The LSP CIA player has no OS restoration in its driver and the current
LSP `p61End` is empty; that is a candidate for the follow-up investigation,
not a proven diagnosis. Normal/packed `out/hexagon` artifacts contain the
motor fix; the baseline disk is an ignored verification artifact only.


## LSP shutdown and return-to-OS cleanup

The LSP exit wrapper was empty and excluded from main's shutdown condition.
Its level-6 vector was therefore left pointing into the executable after DOS
unloaded it. Startup also ran before TakeSystem, contaminating the saved OS
interrupt/DMA state. These are now corrected:

- Start LSP after takeover, passing the detected VBR rather than assuming zero.
- Preserve D2 and A4 as well as the existing saved registers across LSP init;
  the underlying music initializer modifies both.
- Call the exported stop routine on exit. Mask EXTER first, stop both CIAB
  timers, disable/acknowledge CIA interrupts, then stop all four audio DMA
  channels so timer B cannot restart them after shutdown.
- Save/restore the OS level-6 vector, CIAB interrupt mask, timer controls/counts
  and audio-filter bit while custom interrupts are disabled. The original
  OS vector is back before interrupts are enabled.
- Release all allocated HUD sprites, three bitplanes and the copper buffer
  after FreeSystem has restored OS display DMA.

The CIA interrupt enable mask is obtained through AbleICR(resource,0), not a
hardware ICR read (which returns pending flags). See the
[CIA resource documentation](https://wiki.amigaos.net/wiki/Cia.resource).
Timer counters are restored from their paused values; the hardware's write-only
reload latches cannot be recovered this way, so arbitrary third-party periodic
CIA users are not promised exact phase/period preservation across takeover.

Normal and packed release builds pass, including both no-cheat audits.
Disassembly confirms the wrapper preserves D2/A4 and calls the stop routine.
FS-UAE PAL A500, Kickstart1.3, 512 KB Chip/no expansion: boot, play, exit to
AmigaDOS, reload from floppy, play again and exit all pass without a Guru.
[First clean exit](scratchpad/fsuae/pal512/fs-uae-crop-2609191232-04.png),
[reloaded gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191234-02.png),
[second clean exit](scratchpad/fsuae/pal512/fs-uae-crop-2609191235-01.png).
This resolves the previously recorded reload failure on that configuration.
A second floppy reload also reaches the title and exits cleanly (three launches
within one emulator boot): [third title](scratchpad/fsuae/pal512/fs-uae-crop-2609191236-01.png),
[third exit](scratchpad/fsuae/pal512/fs-uae-crop-2609191237-01.png).

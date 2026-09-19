# PC accuracy implementation progress

Updated 2026-09-19. See [the implementation plan](PC_ACCURACY_PLAN.md).

Current milestone: PC-unit walls, collision, exact waves, all six normal/hyper
launch profiles and both automatic stage handoffs are connected to playable
builds. The 193,479 saved wave/selector cases, 21,900 palette ticks and 2,048
opening/handoff ticks pass. Runtime six-profile selection, session unlocks and
per-mode records now have 1,992 native reference cases. Disk persistence,
scripted ending and remaining presentation fidelity are still unfinished.
Immediate starts, angle-preserving retries and the death/retry gate now match
1,698 native snapshots. Death wall movement and polygon restoration now match
17,280 native ticks; camera, flash and audio presentation remain unfinished.

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


## HUD priority above walls

Corrected the copper BPLCON2 value from $0044 to $0024. Normal single-playfield
sprite priority is controlled by PF2P, which was previously zero (playfield
in front of every sprite). Both priority fields now contain 4, placing all
HUD/title sprite pairs in front of the playfield. Removed the incorrect
comment claiming PF2P only applies to dual-playfield displays.
See the [Hardware Reference Manual, priority control register](https://www.theflatnet.de/pub/cbm/amiga/AmigaDevDocs/hard_7.html).

Normal and packed release builds pass, including the no-cheat audits.
FS-UAE PAL A500, 512 KB Chip: [wall crossing behind the complete timer at
2.13 seconds](scratchpad/fsuae/pal512/fs-uae-crop-2609191244-08.png)
confirms the priority fix during gameplay. Title and game-over banners also
render correctly. The verification emulator was closed.


## PC palette adaptation to the two-colour playfield

Replaced the fixed purple/orange playfield and one-frame beat recolouring
with the PC's palette slots0 (background) and2 (primary wall), keeping the
one-bitplane renderer. Player and hub share the wall colour; independent
sector shades/highlights are the explicit two-colour compromise. HUD sprites
retain their black/white colours and priority above walls.

The new portable pc_palette module implements the original integer RGB
interpolation, endpoint holds, pending scheme-change state machine and
normal-stage palette milestones. Hexagon starts red/yellow; Hexagoner uses
orange/green against black; Hexagonest cycles six colour schemes. The 60/120
second scheme changes are implemented for all three supported stages. RGB is
quantized only at output, to the nearest 4-bit OCS channel. Palette updates
run at the simulation's60 Hz cadence independently of display rate and music
BPM. Each new run resets palette phase; title/ready animation does not advance
stage milestone score. The existing brief death flash now sets both colours
white, matching the PC flashlight operation.

Verification executes the original owned PC x86-64 palette routines in private
memory, with no game startup. It exports14 scheme endpoints and21,900 ticks
of original gamelogic palette state/RGB. The target implementation matches
all of these ticks, including Hexagonest's two-second changes and both long
milestones, before independently checking OCS rounding. The fixture reproduces
byte-for-byte with the checked-in, executable-hash-guarded capture tools.
See [palette reference and limitations](tests/PALETTE_REFERENCE.md).

`make test` passes: palette reference, core/geometry,9,159 native wave cases,
184,320 native selector cases and768 long stress runs. Normal/packed releases
for all three supported start stages build and pass the no-cheat audits.
Default `out/hexagon` artifacts have been rebuilt as the normal stage0 release.
FS-UAE PAL A500,512 KB Chip-only: Hexagon and Hexagoner boot and play with the
new palettes; captured walls pass behind the readable timer:
[Hexagon](scratchpad/fsuae/pal512/fs-uae-crop-2609191254-01.png),
[Hexagoner](scratchpad/fsuae/pal512/fs-uae-crop-2609191255-02.png).

This does not add hyper/stage-to-stage gameplay progression, independent
sector colours, the render-time death-glow override, or exact PC flash timing.
Those remaining presentation differences are documented rather than counted
as verified PC parity. The accuracy plan now records the user's two-colour
constraint instead of proposing additional playfield bitplanes.

Hexagonest also boots and plays on the same512 KB configuration:
[green walls and readable HUD](scratchpad/fsuae/pal512/fs-uae-crop-2609191258-06.png),
[start of the two-second colour transition](scratchpad/fsuae/pal512/fs-uae-crop-2609191258-08.png),
[full-white death flash](scratchpad/fsuae/pal512/fs-uae-crop-2609191258-09.png),
[purple palette after the pending fade completes](scratchpad/fsuae/pal512/fs-uae-crop-2609191258-11.png).
All verification emulator instances were closed.


## Sixth batch: hyper entry and automatic stage handoffs

Added a portable pc_progress wrapper around the shared stage selectors. All
three hyper profiles now have their verified wave counter, one-shot opening
wave93/94/95, speed33/33/40, turn rate7/7/9 and palette3/6/9. A60-second
*effective* score offset drives difficulty and palette changes while the HUD
continues to show survival time from zero. Initial cosmetic rotation selection
preserves its source draw range and reject-the-prior-mode behavior.

Normal and hyper stage0/1 runs automatically continue into the next stage when
effective score is strictly greater than10800. The original program emits the
*outgoing* stage's hyper opener on the boundary tick:93 into stage1,94 into
stage2. The new wrapper preserves this ordering, RNG consumption, shape count,
player angle and displayed score, while clearing old walls/waits/morphs. The
following tick uses the next selector and its remapped score. Stage2 turning
changes to9 degrees/tick. Palette changes preserve interpolation phase, and
PAL multi-tick frames consume each transition immediately rather than losing it.

`PC_START_HYPER=1` selects a hyper launch alongside `PC_START_STAGE=0/1/2` until
the runtime selection menu lands. Full hyper titles fit the existing sprite
canvas; the longest title uses a half-width space. Normal launches remain the
default. See [progression specification and build instructions](tests/PROGRESSION_REFERENCE.md).

Verification:

- 2,048 executed-PC opening/boundary ticks match, including wall-record hashes,
  wave/shape counters, delays, speed, turning, profile/score restoration and RNG
  consumption. Cases cover immediate and pending-delay handoffs, and the second
  handoff in a Hexagon run. The hash-guarded capture tool reproduces the fixture
  byte-for-byte. These are bounded synthetic traces, not complete PC replays.
- Twelve native cases are included in the shared host/68000 self-test.
- 384 forced-survival runs exercise 384 handoffs with no overflow. Peak wall
  counts for normal/hyper stages0,1,2 are 50/49, 61/51, 44/44. Stress runs stop at
  stage2 effective score7200, before unimplemented ending behavior.
- The existing normal selector, generator, collision/projection and palette
  suites pass. Six palette launch states now also match native RGB endpoints.
- FS-UAE PAL512 KB Chip-only passes target checks and runs Hyper Hexagonest:
  [full title and PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191333-01.png),
  [gameplay PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191334-01.png).
- FS-UAE NTSC512 KB Chip+512 KB Slow passes target checks and the new hyper
  first-wall live invariant (first 80 ticks at speed 40 from distance 4375):
  [NTSC gameplay PASS](scratchpad/fsuae/ntsc/fs-uae-crop-2609191337-01.png).
- All six normal/packed PAL profile builds pass, including no-cheat audits.
  `out/hexagon`, `out/hexagoner`, `out/hexagonest`, `out/hyper_hexagon`,
  `out/hyper_hexagoner`, `out/hyper_hexagonest` artifacts are uninstrumented
  releases. The final default objects/executable were rebuilt for normal Hexagon.

Next: six-level selection, unlocks and per-mode session records, then the exact
menu/restart/death paths and scripted ending. This batch does not implement
original track changes, camera tilt/cue effects or full ending behavior; it does
not claim complete PC parity merely because the hyper starts are playable.

Final release smoke check: uninstrumented Hyper Hexagon boots and plays on
PAL A500 with 512 KB Chip RAM: [title](scratchpad/fsuae/pal512/fs-uae-crop-2609191343-01.png),
[gameplay at 1.93 seconds](scratchpad/fsuae/pal512/fs-uae-crop-2609191344-01.png).
All verification emulator instances were closed. The final `make test` passes.

## Seventh batch: runtime selection and session records

All six profiles now share one executable. Left/right or joystick rotates the
selection pointer using the PC's ten-tick moves, including held-repeat, release
and reversal behavior and left priority when both directions are held.
Space/fire starts an unlocked profile. Each normal profile starts unlocked;
completing it at tick 3601 unlocks the corresponding hyper entry for the session.
The selected profile determines the opening speed, palette, selector and turn
rate at runtime. Build profile flags choose the initial entry without bypassing
locks.

Records update during play, including the final collision tick, and survive
abandoning a run or changing profiles. Automatic stage handoffs keep recording
against the original selected profile. The selector alternates its name with
its session best, or LOCKED. Immutable startup sprite canvases avoid modifying
active sprite data; cleanup frees every new canvas. The smaller menu zoom keeps
the selection pointer visible within the 200-line viewport.

`make test-menu` compares 1,992 executed-PC cases: 1,920 movement ticks, 48 lock
confirmations, six record/completion slot checks and 18 completion-boundary
cases. The native capture is hash-guarded and reproduces the saved fixture
byte-for-byte. Shared host/68000 checks cover wraps, ties, shorter retries,
independent records/unlocks and long-run attribution. The full host regression
suite passes. See [specification and remaining limits](tests/MENU_REFERENCE.md).

Records/unlocks are session-only: no disk save is claimed. The compact menu
layout, cycling name/record display, menu palette resets and existing ready/death
presentation remain Amiga adaptations. PC unlock announcements/automatic
selection, options, persistence, camera/cues and scripted ending remain pending.

Target verification:

- PAL A500, 512 KB Chip-only: shared checks pass, selector/pointer and gameplay
  run; [menu PASS](scratchpad/fsuae/pal512/fs-uae-crop-2609191406-01.png).
- NTSC A500, 512 KB Chip + 512 KB Slow: runtime selection starts Hexagoner,
  passes its live first-wall invariant and retains its 5.50-second record on
  return; [record](scratchpad/fsuae/ntsc/fs-uae-crop-2609191415-05.png).
  Selecting Hexagonest shows its independent zero record:
  [other profile](scratchpad/fsuae/ntsc/fs-uae-crop-2609191415-07.png).
- Locked Hyper Hexagon displays [LOCKED](scratchpad/fsuae/ntsc/fs-uae-crop-2609191415-09.png)
  and remains in selection after confirmation:
  [rejected start](scratchpad/fsuae/ntsc/fs-uae-crop-2609191415-11.png).
- Native unlock/boundary cases and shared 68000 tests verify completion flags;
  this session did not include a full 60-second interactive unlock run.
- Normal and packed PAL release builds pass the no-cheat audit. The diagnostic
  NTSC image is `out/hexagon_menu_ntsc.adf`; the default release is
  `out/hexagon.adf` (all six profiles), with `out/hexagon_packed.adf` also built.

The macOS menu helper uses 80 ms held keys and explicit screenshots. Initial
instantaneous arrow automation did not reliably cross a hardware input poll;
only the held-key captures above are used as runtime profile-switch evidence.

Final uninstrumented PAL release smoke check on 512 KB Chip-only A500:
[Hexagoner selected at runtime](scratchpad/fsuae/pal512/fs-uae-crop-2609191417-01.png),
[gameplay](scratchpad/fsuae/pal512/fs-uae-crop-2609191417-04.png), and
[clean return to AmigaDOS](scratchpad/fsuae/pal512/fs-uae-crop-2609191417-08.png).
All verification emulator instances were closed.

Next: exact ready/death/retry and unlock presentation, saved records/unlocks,
then the remaining scripted ending and camera/cue work.

## Eighth batch: start and retry mechanics

Removed the added 90-tick ready wait. Accepted confirmation now resets the run
and executes its first logic tick immediately, preserving the player's current
and previous angles. Menu movement is not applied twice on that tick. The live
score is maintained in the portable lifecycle state and frozen through death.

Replaced the fixed 60-tick retry delay with the PC's death timer and transition
extent gate. With extent 40 at collision, extent reaches 320 on the 72nd
subsequent tick; held confirmation retries on tick 73 because input precedes
logic. A tap only on tick 72 is rejected rather than queued. Removed the
unverified random death shake that consumed gameplay RNG.

Verification:

- 1,698 native snapshots match: both menu entry and result-screen retry for
  all six profiles at seven angles, plus 85-tick death sequences with no input,
  held confirmation and an early tap. The capture reproduces byte-for-byte.
- 24 native boundary states are part of the shared host/68000 tests. The full
  host regression suite passes; the expanded lifecycle fixture passes too.
- PAL A500, 512 KB Chip-only: target PASS, immediate start with score already
  advancing in [the first capture](scratchpad/fsuae/pal512/fs-uae-crop-2609191426-01.png),
  and held-Space retry in [the later capture](scratchpad/fsuae/pal512/fs-uae-crop-2609191426-04.png).
  The live first-wall invariant continues to pass after retry.

See [source addresses, boundaries and limits](tests/LIFECYCLE_REFERENCE.md).
The first-run tutorial and pending unlock announcement paths are excluded from
these fixtures. Full PC death visuals (wall retreat, morph continuation, camera,
flash, audio fade/cues) remain pending. The existing result banner is still an
Amiga presentation adaptation. Records/unlocks still do not persist to disk.

NTSC A500 verification (512 KB Chip + 512 KB Slow) also passes, including
runtime Hexagoner selection, [immediate gameplay](scratchpad/fsuae/ntsc/fs-uae-crop-2609191428-03.png),
and [immediate retry gameplay](scratchpad/fsuae/ntsc/fs-uae-crop-2609191429-01.png).
PAL and NTSC verification instances were closed. Both normal and packed PAL
release builds pass the no-cheat audit; default artifacts are `out/hexagon.adf`
and `out/hexagon_packed.adf`. `out/hexagon_lifecycle_ntsc.adf` is diagnostic.

Next: finish the PC death presentation and unlock transitions, implement saved
records/unlocks, then continue with scripted ending and camera/cue parity.

## Ninth batch: death wall movement and polygon restoration

The death path now continues polygon interpolation, freezes wall distances
until death timer 60, then retreats walls by 200 PC units per tick. It preserves
widths/active flags and command-marker waits. Inactive holes move with the record
array; inactive tails are trimmed only after movement. The result renderer
continues to draw remaining walls rather than removing them at a mode switch.

Hexagon returns toward six sides once the result extent opens, completing any
in-progress shrink/grow first. Native continuation probes exposed another
boundary: once the PC would grow extent past 320, it clears continuation flags
before stage-specific death logic. The port therefore applies the originally
selected stage's polygon restoration from that point, including after either
Hexagon handoff.

Verification: 17,280 native ticks match wall hashes/counts, polygon arc and
state, marker wait, frozen score and death/extent counters. Cases cover six
profiles, six continuation setups, six polygon trajectories and two initial
death timers over 120 ticks. 181 boundary cases are also included in shared
host/68000 checks. The full host regression suite passes. See
[death reference and limitations](tests/DEATH_REFERENCE.md).

Still pending: PC camera/field motion, flash and audio fade/cues, tutorial,
unlock announcement/selection transitions, saved records/unlocks and scripted
ending. This batch implements death geometry, not full presentation parity.

PAL A500 verification, 512 KB Chip-only: target PASS and captured death
sequence. The score remains 2.43 seconds while the walls are still present
([first frame](scratchpad/fsuae/pal512/fs-uae-crop-2609191441-04.png),
[following frame](scratchpad/fsuae/pal512/fs-uae-crop-2609191441-05.png));
then walls have retreated out of view at
[results](scratchpad/fsuae/pal512/fs-uae-crop-2609191441-06.png).
The native capture reproduces the fixture byte-for-byte. Both normal and
packed PAL release builds pass the no-cheat audit.

NTSC A500 (512 KB Chip + 512 KB Slow) also passes all shared target checks.
The recorded run freezes at 3.95 seconds during death:
[wall hold](scratchpad/fsuae/ntsc/fs-uae-crop-2609191442-07.png),
[retreat/results](scratchpad/fsuae/ntsc/fs-uae-crop-2609191442-09.png).
All verification emulator instances were closed. The default normal/packed
PAL releases are `out/hexagon.adf` and `out/hexagon_packed.adf`;
`out/hexagon_death_ntsc.adf` contains diagnostic self-tests.

## Menu high score and text together

The attract/menu screen now keeps the selected profile's session best visible
above its name. Locked entries still alternate their name and `LOCKED`, with
the score present throughout. Gameplay and result banner timing is unchanged.

The copper draws the upper score, waits until its sprite terminators have been
fetched, then reloads all eight pointers and POS/CTL registers for the lower
banner. Lower pointers skip the headers supplied by the copper. Merely changing
pointers after the terminator did not restart the sprites in FS-UAE; explicit
control-register reloads resolve that. Existing immutable sprite buffers are
reused, with no extra allocation. The HUD emits 110 words in every mode; the
normal complete copper list occupies 276 bytes of its existing 1,024-byte buffer.
Scene and HUD colours are set before the mid-display WAIT.

PAL release verification in FS-UAE, A500 with 512 KB Chip and no expansion RAM:
[earned 3.95-second best alongside HEXAGON](scratchpad/fsuae/pal512/fs-uae-crop-2609191455-02.png),
[long profile name](scratchpad/fsuae/pal512/fs-uae-crop-2609191455-03.png), and
[LOCKED with score](scratchpad/fsuae/pal512/fs-uae-crop-2609191455-04.png).
The death/result sequence also retains its
[GAME OVER banner](scratchpad/fsuae/pal512/fs-uae-crop-2609191454-10.png).
Normal and packed PAL release builds pass the no-cheat audit.

NTSC A500 (512 KB Chip + 512 KB Slow) reports target self-test PASS, with
[both rows visible](scratchpad/fsuae/ntsc/fs-uae-crop-2609191456-01.png) and
[selection updated to HEXAGONER](scratchpad/fsuae/ntsc/fs-uae-crop-2609191456-03.png).
Verification instances were closed. Default normal/packed PAL releases remain
`out/hexagon.adf` and `out/hexagon_packed.adf`; the NTSC diagnostic build is
`out/hexagon_menu_ntsc.adf`.

### Locked names stay hidden

Locked selections now show `LOCKED` continuously, revealing their profile name
only when `game_selection_locked()` becomes false. This replaces the alternating
name/lock presentation described above; the high score remains visible.
PAL A500 512 KB Chip-only FS-UAE captures show `LOCKED` both
[immediately after selection](scratchpad/fsuae/pal512/fs-uae-crop-2609191458-01.png)
and [two seconds later](scratchpad/fsuae/pal512/fs-uae-crop-2609191458-02.png).
Normal and packed releases were rebuilt and pass the no-cheat audit.

## Sound effects and speech

Original PC clips now convert offline to word-aligned 8 kHz PCM. AUD0 remains
reserved for music; AUD1–3 share a one-shot effect/voice pool and a common
level-4 dispatcher. Hooks cover existing menu movement, begin/start, record
crossing, rank thresholds, ordinary death, delayed game-over, completed-result
voices, continuation and the startup announcement. See
`soundtrack/SOUND_EFFECTS.md` for exact source anchors and limitations: PC
settings/leaderboard and scripted ending scenes remain unimplemented, and
three hardware voices limit simultaneous effects. The combined music/effects
trial targets 512 KiB Chip plus 512 KiB expansion; effects alone are the default
build. Host boundary and existing menu/lifecycle/death fixture checks pass.

PAL FS-UAE validation: the combined build reached 45.50 seconds with zero music
underruns while rank clips played, and returned cleanly to AmigaDOS. The default
effects-only build also booted and ran with 512 KiB Chip and no expansion.
See `soundtrack/SOUND_EFFECTS.md` for evidence, the corrected one-shot DMA issue,
and the remaining NTSC/listening/ending-scene limitations.

## Soundtrack-synchronized radial pulse

Replaced the 132-BPM camera zoom with the PC cue-driven additive displacement
for hub, player and both wall edges. Extracted the original 11,441 Courtesy
cues; verified the source MP3 timing offset at four positions (51 ms).
The playback clock follows the DMA-owned buffer, with raster interpolation,
retry reset and real PCM loop length. No audio decoding or analysis runs live.
PC per-stage envelope arithmetic and the scheduler rotation pulse of 12 are
implemented. Exhaustive pulse and existing lifecycle/menu/SFX host tests pass.
See `soundtrack/VISUAL_SYNC.md`; stage-2 camera-freeze/ending integration,
other soundtracks and full perspective remain outstanding.

FS-UAE trials on 512 KiB Chip + 512 KiB Slow: PAL reached 45.51 seconds,
NTSC 45.56 seconds, both with zero music underruns. Final NTSC geometry keeps
player size constant while moving its centre. Release build/audit passed;
`out/pcm_pulse.adf` contains music, speech/effects and synchronized pulsing.

## PAL/NTSC display-window alignment

Fixed `DISPLAY_HW_Y`, which incorrectly used SCREEN_WIDTH and a PAL-only
reference for both standards. The 200-line NTSC viewport now starts at line 44
instead of 76 (stop 244 instead of 276, beyond the NTSC frame). PAL now centres
200 lines within its 256-line area, starting at 72 instead of 76. Bitplanes,
sprite positions and HUD multiplex waits share this origin. DIWSTOP explicitly
masks its low coordinate bytes, avoiding a negative shift for NTSC stops below
256. Music-enabled PAL and NTSC release builds pass the cheat audit.

NTSC FS-UAE menu/gameplay verification passed with the music/effects build
(512 KiB Chip + 512 KiB Slow), with zero reported music underruns. Evidence:
`scratchpad/fsuae/ntsc/fs-uae-crop-2609192048-02.png` (menu) and
`fs-uae-crop-2609192049-02.png` (gameplay). The shared origin moved both sprite
HUD and scene up 32 lines. Updated output: `out/hexagon_ntsc.adf`.

## One executable for PAL and NTSC

Added startup raster measurement after takeover, before Copper, HUD or audio
initialization. A complete field selects the nominal 50/60 Hz simulation input,
PAL/NTSC visible area and music/SFX Paula periods. The cue clock uses the
measured field length. There are no remaining `TARGET_NTSC` branches in C code;
both standards use the same ADF. The unused aspect macro was removed, preserving
the renderer's existing common pixel geometry.

`make test-video test-pulse test-pcm-lifecycle test-sfx` passes, including
312/313-line PAL and 262/263-line NTSC configurations. Both the combined-audio
and default effects-only builds pass the release cheat audit.

FS-UAE boot/menu/gameplay checks passed in both modes using byte-identical
`out/hexagon.adf` (SHA-256
`f7fc6c2711ad300c8608bef36324e62aa20f13e0b725c715e9964f83c6786333`).
The HUD/playfield selected the correct origins; both showed zero music
underruns. Gameplay captures:
`scratchpad/fsuae/pal/fs-uae-crop-2609192053-03.png` and
`scratchpad/fsuae/ntsc/fs-uae-crop-2609192055-03.png`.
These are short startup/gameplay checks, not new full-song stress trials.

### AmigaOS video-standard selection

Replaced raster measurement with `(GfxBase->DisplayFlags & PAL) != 0`, using
the SDK's PAL flag immediately after opening graphics.library, before system
takeover. Removed the extra field-wait/measurement function. The universal
binary still selects all display/audio timings together; the cue clock uses
nominal 312/262-line fields. Host timing/pulse checks and release build pass.
This supersedes the raster-detection mechanism described above.

## Release memory cleanup

Removed the unused menuselect payload and exact zero tails from the SFX bank,
unreachable per-slot HUD glyph allocations, legacy codec/interpolation state
from PCM playback, and the release audio diagnostic overlay. Accepted music
quality and four DMA buffers remain unchanged. `AUDIO_DIAGNOSTICS=1` retains the
overlay for trials.

Compared with the previous universal release, executable size falls from
477,228 to 465,072 bytes. Accounted runtime RAM falls by 13,436 bytes, including
10,952 mandatory Chip bytes. See `MEMORY_USAGE.md` for allocation categories,
OS/stack exclusions and further candidates that need separate validation.

Host tests pass. The compact PCM state reproduces 2,324,096 samples exactly
across a full-song loop with canaries intact. Every retained SFX audible sample
is unchanged and aligned; only digital-zero suffixes were removed. PAL FS-UAE
boot, timer/HUD and gameplay check passed (music/effects release, 1 MB):
`scratchpad/fsuae/pal/fs-uae-crop-2609192103-03.png`. This release omits the
counter overlay, so this screenshot does not measure underruns.

## Default execram ADF

`make` and `make adf` now boot the execram-packed executable from the canonical
`out/hexagon.adf`. The `_packed.adf` name is an identical compatibility copy;
`make adf-unpacked` explicitly builds the diagnostic alternative. Incremental
builds skip packing and disk creation when their inputs are unchanged.

Execram merges mixed-memory hunks into Chip RAM. Packing the old combined
executable directly failed to load on the 512 KB Chip + 512 KB expansion A500.
The CPU-only music prefix is therefore an ADF file, `music.pcm0`, loaded and
validated before takeover and freed on exit. The DMA bank remains embedded.
No music quality changes, gameplay decompression or gameplay disk access were
introduced. Direct executable launches need the bank in the current directory.

The combined release now contains a 173,308-byte packed executable plus a
137,610-byte music file: 310,918 bytes of payload, about 33% below the preceding
465,072-byte executable. The ADF itself remains the standard 901,120 bytes.
See `MEMORY_USAGE.md` for the new Chip/expansion distribution and startup costs.

Execram's host decompression check, the release cheat audit, video/pulse/SFX/
PCM lifecycle tests, and the 2,324,096-sample PCM equivalence check pass.
Both music and effects-only packed disk targets build successfully. The music
disk passed PAL FS-UAE boot, gameplay and return-to-menu checks with 512 KB
Chip + 512 KB expansion RAM; gameplay capture:
`scratchpad/fsuae/pal/fs-uae-crop-2609192111-03.png`. This is a short smoke check,
not a new full-song or underrun measurement.

### Execram 1.3.0: embedded music restored

Retested after the fix for https://github.com/going-digital/execram/issues/2.
Execram 1.3.0 preserves ordinary and Chip memory classes with cross-region
relocations. Removed the external music loader and ADF sidecar staging; both
PCM banks are embedded again. Both bank files are explicit object dependencies.
The packed ADF remains the default, and direct launches need no music file.
Use execram 1.3.0 or newer for this layout.

The 465,072-byte release packs to 256,500 bytes, 54,418 bytes less than the
previous packed executable plus sidecar. Resident regions are 222,904 ordinary
bytes and 242,604 Chip bytes; temporary depacker scratch is 1,580 bytes.
With runtime allocations, accounted RAM is 497,180 bytes, including 274,276
mandatory Chip bytes (86,416 fewer than the sidecar workaround).

Host video, pulse, SFX and PCM lifecycle tests pass, as do the release cheat
audit, execram self-check and full-song PCM sample/canary check. The new disk
boots and runs gameplay in PAL FS-UAE with 512 KB Chip + 512 KB Slow RAM:
`scratchpad/fsuae/pal/fs-uae-crop-2609192215-03.png`. This supersedes the
sidecar requirement above; it is a short smoke check, not a full-song audio
or underrun measurement.

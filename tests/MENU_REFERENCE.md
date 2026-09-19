# PC menu and session records

Reference: owned desktop Mach-O SHA-256
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`.
The capture script checks the full fingerprint before loading executable segments
into private memory. It never modifies the original executable or invokes its
application startup. Audio, palette, score API and save-file calls are stubbed.

## Verified rules

The profile index is `stage + 3 * hyper`. Normal Hexagon, Hexagoner and Hexagonest
are all initially selectable. Each corresponding normal completion enables its
hyper profile independently. Hyper completion does not unlock another normal
level. The menu's wedge order is `[0,5,4,3,2,1]` in profile indices.

`superhex::gameinput` at `0x100052980` moves six degrees per nominal tick for ten
ticks, then snaps to the next wedge's centre. Left wins if both directions are
held. There is an eight-tick input cooldown; an active ten-tick move finishes
even if its key is released or reversed. Held input can begin another move on
the next tick after completion. Confirmation uses the current wedge, including
when the pointer is still moving. Locked selection is rejected.

`setbesttime` at `0x10000c450` and `getbesttime_stage` at `0x10000c480` use six
separate slots at gameclass offset `0x2990`. `gamelogic` at `0x100056f50` updates
the selected profile's record as the displayed score advances, before hyper
score offsets and continuation remapping. The record is not restricted to runs
that end by collision. `winlevel` at `0x100009b50` writes separate normal/hyper
completion flags. At nominal dt=1, its completion threshold is strictly greater
than 3600 ticks (first completion at tick 3601). The constructor's threshold
initialization is at `0x100003227`; the logic's threshold comparison dispatches
`winlevel` after the preceding ranks have been reached.

## Executed reference and tests

`python3 tools/capture_pc_menu.py [path/to/SuperHexagon]` reproduces
`tests/fixtures/pc_menu_native.txt.gz` as plain text. `make test-menu` compares:

- 1,920 menu ticks: all six initial wedges, held left, held right, both held,
  and a tape containing reversal and release; angle, wedge, motion and cooldown.
- 48 confirmations: every normal-completion mask and every wedge, including
  rejected hyper entries and accepted profile mapping.
- Six independent record writes/completions: all record slots and completion
  flags, with the native save operation suppressed.
- 18 live logic boundary cases: all six profiles immediately below, at and
  above the completion threshold. These synthetic cases seed rank four to
  isolate the completion branch; they do not replay the preceding ranks.

Shared host/68000 checks also exercise wrapping, ties, shorter retries, isolated
records and unlocks, and retaining a long continuation under its selected
profile. The native traces do not cover the complete desktop menu, options,
leaderboards, unlock announcements, fractional dt or persistence.

## Amiga integration and intentional limits

Left/right arrows or the joystick select a profile; Space/fire starts an
unlocked selection. Escape returns from play/results to selection; Escape
again exits. The menu continuously shows the selected profile's session best above its name.
Locked entries display `LOCKED` continuously until unlocked, then reveal their
profile name. The best remains visible in either state. The copper reloads all eight sprite pointers and position/control registers
between the score and banner rows, without allocating extra sprite data.
After the upper terminators have been fetched, the lower pointers address pixel
data directly and the copper supplies POS/CTL. This follows the register/DMA
model in the [Amiga Hardware Reference Manual, Sprite Hardware](https://www.amigarealm.com/computing/knowledge/hardref/ch4.htm). Six immutable title canvases
and a locked banner are allocated at startup, avoiding writes to active sprite
DMA data. The six additional canvases cost 1,920 bytes of Chip RAM plus
allocator overhead. The pointer is kept within the 200-line display by reducing menu zoom.
This layout is an adaptation to the existing two-colour sprite renderer, not a
reproduction of the PC menu artwork or camera.

Records and unlocks last for the current program session only. Disk persistence,
the PC's unlock announcements/automatic selection, exact ready/death/retry
presentation, camera effects and scripted ending remain pending. Runs keep
updating their selected profile's record through automatic stage handoffs.
`PC_START_STAGE` and `PC_START_HYPER` now choose only the initial menu entry;
they do not bypass locks. All six profiles ship in one executable.

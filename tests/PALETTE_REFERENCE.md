# Two-colour PC palette reference

The source is the owned Steam macOS x86-64 PC executable, SHA-256
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`.
Windows builds have not been independently checked.

## Mapping to one bitplane

Amiga COLOR00 uses PC palette slot 0 (the `ofBackground` colour in
`superhex::gamerender`, 0x10005f8a0). COLOR01 uses slot 2 (primary walls in
`graphicsclass::compose3dframe`, 0x1000490f0, and `draw3dscene`, 0x10004b710).
The Amiga player and hub necessarily share that foreground. Alternating
background sectors, alternating wall shades and player-sector highlights
cannot retain independent colours with this renderer. HUD sprites remain
black/white and above the playfield.

This deliberately selects real PC colours rather than averaging the scene
or synthesizing a brightness ramp. For example, initial Hexagon endpoints:

| Colour | Start RGB | End RGB | Start/end OCS |
|---|---|---|---|
| Background | 71,17,5 | 61,51,6 | 410 / 430 |
| Main wall | 193,17,30 | 215,189,17 | B12 / DB1 |

Interpolate each original 8-bit channel with signed integer division toward
zero, then round to the nearest OCS channel level: `(channel+8)/17`.
No dithering, extra bitplanes or extra gameplay RNG draws are introduced.

## Scheme selection and timing

`setpal` (0x10004dab0), `autopal` (0x10004ce20), `brightendpal`, and
`updatepal` (0x10004c8b0) supply the endpoints; `gamelogic` (0x100056f50)
drives the blend and scheme transitions. Normal stage entry is confirmed in
`gameinput` (0x100052980), not inferred from the palette's appearance.

- Hexagon starts at scheme 0, changes to 3 after score 3600, then 1 after 7200.
- Hexagoner starts at 5, changes to 6 after 3600, then 7 after 7200.
- Hexagonest starts at 10, cycles 10–15 at 120-tick milestones through 3600,
  changes to 9 after 3600 and 30 after 7200.
- Ordinary interpolation advances by four units per 60 Hz tick, clamped at
  0/255 with the original idle endpoint tick. Scheme changes first subtract
  25 after the ordinary update, capture the displayed RGB with setpal(1000),
  then add four after ordinary updates until restoring the target scheme.
  This ordering is not equivalent to a uniform fade or a simple triangle wave.

The standalone palette resets at the start of each playable run; menu/ready
frames animate the selected palette without advancing milestone score.
Normal/hyper entry and two automatic stage handoffs are now implemented; see
[PROGRESSION_REFERENCE.md](PROGRESSION_REFERENCE.md) for their verified scope.
The scripted ending remains pending.
The PC render-time death-glow override and exact flash timing remain part of
the pending presentation work. The existing six-tick death flash now correctly
sets both scene colours to white, matching the PC flashlight colour operation.

## Reproduction and tests

`python3 tools/capture_pc_palette.py > /tmp/pc_palette_native.txt` checks the
binary hash, compiles the x86-64 probe and executes the original functions in
private memory. It does not launch the game or modify its executable.
The probe emits 14 schemes' selected endpoints, then 7300 consecutive ticks
for each of the three stages, starting with blend/direction zero. Game logic
has a very large spawn delay and no walls; audio/score side effects are stubbed,
but palette routines run unmodified. Score is supplied before its increment.

`tests/fixtures/pc_palette_native.txt.gz` preserves this output. `make test-palette`
compares every RGB component, scheme, pending-change state, blend and direction
against it (21,900 ticks), then independently checks OCS rounding. `make test`
also runs this comparison. The fixture includes both long-stage milestone
changes and every Hexagonest hue transition. It can run without the PC game.

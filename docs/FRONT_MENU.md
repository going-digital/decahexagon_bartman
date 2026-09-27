# Front-end menu

Boot opens Start / Options / Credits. Left/right (keyboard or joystick) changes
selection on a fresh direction press. Space, Return or fire confirms.

Start opens the existing six-profile selector. Escape from that selector returns
to the top-level menu. Escape during a run/results returns to the selector, as
before. At the top level, Escape retains the platform's existing exit policy:
WHDLoad/OS builds can quit; native floppy boot cannot return to an OS.

Options is an empty placeholder. Credits has five pages containing the authors,
websites, contributors, testers, assistance and calls to action in `credits.md`.
Left/right changes pages; confirmation advances; pages wrap. Escape returns to
the top-level menu. Credits text is compiled into the HUD, not loaded from disk.

Front-end pages share the existing attract state, preserving the resident
soundtrack and save boundaries. They hide timer/banner/player sprites and draw
into the unpublished foreground buffer after blits finish. Menu-only text code
uses size optimisation; gameplay rendering retains its existing settings.
Developer F1-F8 shortcuts remain available from these attract screens.

`make test-front-menu` checks routing, held-direction suppression, credits wrap
and back navigation, and is included in `make test`. Host regression tests and
ADF/WHDLoad distribution checks pass. The enlarged payload also required the
native builder to reserve its fixed boot/save sectors before DosIO file
allocation, instead of marking them after allocation. The existing whole-image
ownership/bitmap check validates disjointness and readback validates every file.

The native A500 cold boot was visually checked: the home menu displays correctly,
right selects Options, and Start enters the level selector. Automated multi-input
runs from saved emulator states did not reliably deliver subsequent confirmation
inputs; Options/Credits routing and page wrap are covered by host tests, but their
interactive visual checks remain for playtesting. Captures are in
`scratchpad/front_menu/`. The current distribution is `out/Hexagon.zip` with
cheats enabled. Thirty disk sectors remain free.

## Cached two-bitplane menus

Front-end pages now enable two bitplanes: plane 0 is the animated scene, plane 1
is the menu bitmap. Colours 2 and 3 are both white so text wins regardless of the
background bit. Gameplay/level selection restores the normal one-bitplane mode.
This is two planes in one four-colour playfield, not the Amiga DUALPF mode.

Three 8,000-byte Chip RAM menu buffers follow the three copper-list slots. The
free slot is updated only when its page/selection key changes. Active/queued
bitmaps are never modified. Steady frames only reuse the bitmap pointer. A
failed optional allocation falls back to the prior drawing path. Menu cache
allocation costs 24,000 bytes; no per-frame allocation occurs.

Uppercase menu headings/entries use compiled 16x16 Bump IT UP glyphs, rasterised
from the installed PC game's bumpitup.ttf by `tools/build_menu_font.swift`.
Credit details retain the smaller font so complete names/URLs fit. Aaron Amar's
font attribution, source URL and CC BY-SA 3.0 notice are included in the ZIP as
BUMP_IT_UP_LICENSE.txt and in assets/fonts. Only the required uppercase glyphs
are embedded, keeping the executable within its fixed resident memory budget.

Full host regressions plus cache ownership/invalidation tests pass. A500 native
screenshots now confirm the home, Options and Credits screens, and Start reaches
the selector. The earlier automated submenu-input limitation is superseded by
these successful captures under `scratchpad/menu_cache/`.

A 99-video-frame PAL A500 OCS sample changes from 12 to 99 background fill
operations: approximately 6.05 to 49.92 background renders/second. One fill is
issued per render, so this is a render-rate proxy rather than exact presentation
FPS. The after sample includes the extra plane's DMA. This is a short emulator
measurement, not a hardware/whole-menu guarantee. See MENU_CACHE_PROFILE.json.
Both distribution builds and ZIP checks pass; out/Hexagon.zip is updated.

## Faster menu changes

A new page/selection is now rasterised once. The other two free cache slots
receive blitter copies of that bitmap instead of independently rasterising it.
The blitter also clears the destination before rasterisation. Every DMA operation
finishes before CPU writes or copper publication; active/queued planes are only
read as copy sources. Unchanged menu frames still perform no bitmap work.

Glyphs now write row masks into one or two bytes instead of testing and writing
individual pixels. The selection underline uses whole bytes. All 45 tested
page/choice/credit combinations are byte-identical to the old renderer, with
buffer guards intact (`make test-menu-raster`). Cache tests verify one draw and
two copies per change, reuse, invalidation and free-slot ownership.

The isolated Musashi 68000 benchmark for two headings and two credit lines drops
from 753,622 to 134,552 cycles: 82.1% fewer cycles, or 5.60 times faster. This
excludes clearing, copying, Chip RAM contention and the additional saving from
rasterising once instead of three times; it is not whole-transition timing.
See `MENU_TEXT_BENCHMARK.json` and `tools/performance/benchmark_menu_text.py`.

Full host regressions and distribution checks pass. Native A500 emulator captures
under `scratchpad/menu_blits/` confirm Home, Options, Credits and the level
selector after these changes. `out/Hexagon.zip` contains the updated build for
hardware playtesting.

## Menu sound effects

Menu movement uses the original PC `menuchoose` clip (effect 12); confirmation
uses `menuselect` (effect 11), while Escape/back uses `rankup` (effect 5),
matching the PC cancellation branches. These IDs match the PC input
handler's movement/confirmation branches in the local decompilation. Credits
page navigation follows the same convention. Held directions and inactive
Options confirmation are silent. Existing level-selector movement and game-start
sounds are unchanged. The previously omitted `menuselect` PCM is now included
in the SFX bank and played through the existing Paula hardware voices.
`test-front-menu` checks movement, confirmation and suppressed repeat/no-op events.
The restored confirmation clip adds 1,726 PCM bytes. Native/WHDLoad flat-image
linking now uses `-z max-page-size=4` to omit the unused ELF text/data page gap; no hardware or
resident memory regions were enlarged. Existing EXE1 relocation, inflation and
resident-budget checks validate the resulting image.

### ADF boot regression after adding menu effects

The larger game file reached sectors 1705–1737. The image builder correctly left
these available after the bootstrap reservation (1661–1704), but the runtime OFS
reader rejected all sectors from 1661 onwards. `check_ofs.py` reproduced the
failure on the released image, stopping after sector 1660. The reader now allows
1705–1737 while still rejecting boot sectors, bootstrap sectors and save tracks
1738–1759. Its malformed-input tests also exercise each reserved range through
the root hash, before any read. The release builder now runs this actual C reader
against every payload after building the ADF, in addition to the Python layout
checks, so an allocator/reader disagreement fails the build.

The corrected release ADF cold-boots on the emulated A500 OCS with 512K Chip
and 512K Slow RAM, reaches Home and reports Courtesy ready. Screenshot and serial
log: `scratchpad/menu_boot_fixed/`. The ADF inside `out/Hexagon.zip` was compared
byte-for-byte with this boot-tested image (SHA-256
`ad8a62ed1212d2b960cba60e5361ea26d5317d1ce0b6f3ff9f6f4896fd3e2ce0`).

## Horizontal Home carousel

Home now centres the selected label at (160,92). The previous/next labels sit
192 pixels to either side, leaving their inner edges visible. Left/right advances
around Start / Options / Credits with an eased slide; further direction presses
are accepted after the slide settles. Confirmation acts on the selected target.
Returning from a submenu snaps to its selected Home item.

A 1,856-byte strip is rasterised once at startup. Only the 640-byte label band
is shifted into the free overlay buffer during animation; title/instructions stay
cached. Idle buffers do no work. The CPU shifts these short rows, avoiding glyph
rasterisation and per-glyph blitter setup; the blitter retains bulk clear/copy
operations. Optional cache allocation failure shows a centred static selection.
`test-menu-carousel` checks all 576 viewport positions against an independent
pixel oracle, write guards, wrapping and monotonic settling in both directions.
Home's static cache key excludes selection and scroll position. Its three static
copies are drawn on entry; navigation updates only the label band, with no full
bitmap clear or copy. Home initialisation clears each buffer with longword CPU stores before drawing
its static text; these clears never occur during a selection slide.

The ADF boots and emulator captures confirm the centred selections, intermediate
slide and wrap in both directions. Some timed emulator captures omit parts of
both static text and background; the saved Chip RAM contains three identical,
complete Home bitmaps and correct copper pointers. This capture/display anomaly
is not yet explained; hardware playtesting should check for visible flicker.

## Level-selection carousel

The six-profile selector now shares the Home carousel: a centred 16x16 level
name, clipped neighbours and eased 192-pixel slides on fresh left/right presses.
A NORMAL/HYPER heading identifies the tier; the selected profile's existing
best-time HUD is retained. Locked levels display their normal-level completion
requirement and cannot launch. Confirmation starts the selected target, retaining
existing track loading, load-retry, unlocking, record and game-start sound rules.
Escape returns Home with the PC back sound; returning from a run keeps the chosen
profile. The old rotating player pointer is hidden on the selector.

Home and levels reuse one 3,008-byte strip (six levels plus wrap padding). Six
obsolete sprite title canvases have been removed. The overlay is now active on
all attract pages; gameplay remains single-bitplane. The six-entry strip and
both-direction wrap are covered by the pixel-oracle and input tests, in addition
to existing progression/record tests.

Validation: full `make test` and the ADF/WHDLoad distribution build pass. A500
emulator captures in `scratchpad/level_carousel/` confirm Hexagon, rightward
Hexagoner selection, leftward wrap to locked Hyper Hexagonest (fire remains on
the selector), returning Home and starting a normal run through game-over.

### Level details and contrast

Normal profiles omit the tier heading. The scrolling level name is followed by
centred `DIFFICULTY: HARD`, `HARDER`, `HARDEST`, `HARDESTEST`,
`HARDESTESTEST`, or `HARDESTESTESTEST`, then `BEST SCORE: SSS.CC`.
The old top-left selector score sprites are hidden. Score changes invalidate
cached menu text even if the selected profile has not changed.
Hyper Hexagoner (profile 4) sets both overlay colours to grey ($555); other
profiles and pages restore white ($fff). This is applied on every copper-list
publication, independent of cached bitmap reuse.

Validation: formatter tests cover all six difficulty strings, absent NORMAL,
HYPER, zero/nonzero/capped best times; cache tests cover changed scores. A500
captures in `scratchpad/level_details/` confirm the normal page, grey Hyper
Hexagoner on white and restored white text after moving to Hyper Hexagonest.
The ADF/WHDLoad distribution build and its payload checks pass.

### Locked carousel entries

Locked profiles use `LOCKED` in the bitmap strip, including the clipped neighbour
previews. Their tier heading, difficulty and best score are hidden; the unlock
requirement and navigation hint remain. The strip and all three overlay caches
include the full unlock mask in their identity, so newly unlocked profiles regain
their names and details. The unused old LOCKED sprite banner was removed.
Tests cover all eight Hyper unlock masks across every viewport position, plus
suppressed heading/difficulty/score in the actual text renderer.

## QR website credits

Credits now use eight pages: Terry Cavanagh, Chipzel, Jenn Frank, Ethan Lee,
Amiga contributors, testers/assistance, font attribution, and calls to action.
Website text is replaced by offline-generated QR codes for the five credited
URLs; the soundtrack call-to-action reuses Chipzel's code. Each code has two
screen pixels per module and a four-module white quiet zone. Credits force both
background colours black, giving a stable black/white scan target independent
of the animated scene. Other pages restore their usual palette.

`tools/build_credit_qr.py` (Python `qrcode` dependency) regenerates the committed
532-byte QR-L bitmap bank in `credit_qr.h`; release builds need no QR encoder.
The actual HUD renderer's output was decoded with macOS Vision for all six
QR-bearing pages and matched the expected URLs. Full host tests pass. HUD and
startup sprite construction use size optimisation to fit the existing resident
image budget; game simulation and polygon rendering retain their settings.

The rebuilt ADF boots successfully; emulator screenshots of author, soundtrack
and font QR pages are in `scratchpad/credit_qr/`. The previously noted timed
capture anomaly still affects some static headings/footer text, while the QR
regions are present. Phone scanning on the target display remains a playtest check.

### Credits source refresh

The current `credits.md` order is reflected in seven pages: Terry Cavanagh,
Chipzel, Jenn Frank, Aaron Amar (font), Ethan Lee (PC port), Amiga port/additional
code, and Playtesting/additional assistance. The removed calls-to-action page
is no longer shown. All credited names and the five website QR destinations are
retained; the font and PC-port QR indices follow their reordered pages.

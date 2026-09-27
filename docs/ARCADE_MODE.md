# Arcade mode

The playtested menus and credits were committed as `4005a17` before this work.

## PC evidence

The local PC disassembly and decompilation provide these rules:

- `gameclass::switchtoarcademode` at `0x100009090` sets all three Hyper unlocks and completion flags, backs up the six normal best times, and clears the active normal best-time slots. See `scratchpad/pc_verification/evidence/binary_disassembly.txt`.
- Options case 4 in `decomp_gameinput_100052980.c` toggles Arcade, loads its scores, and switches between normal and Arcade state.
- `gameclass::checkhighscore` at `0x100045110` indexes five entries per profile. A new time must strictly exceed an entry to be inserted ahead of it; equal scores retain their earlier position. Names and scores shift together.
- The PC input handler bounds names at ten characters. It offers alphabet selection, space, deletion and acceptance. The Amiga supports direct keyboard entry and Return/joystick-fire acceptance.
- `decomp_gamelogic_100056f50.c` checks Arcade scores during the ordinary death sequence. The Amiga opens name entry when that death transition is ready for retry.
- The PC has separate `loadarcadescores` / `savearcadescores` functions. The Amiga currently keeps **session-only** Arcade tables, following the request that Arcade must not change saved state. They survive switching Arcade off/on, but not quitting or rebooting.

The initial request for ten scores was subsequently changed to five to match the PC.

## Amiga implementation

Options contains `ARCADE MODE: OFF / ON`, toggled with Space, Return or fire.
All six profiles are selectable in Arcade. Each profile has five descending time/name entries, displayed below its title on the level selector. Times retain simulation ticks internally and display hundredths.

A qualifying death opens name entry. Type up to ten uppercase letters, digits, spaces or hyphens; Backspace deletes. Return or joystick fire accepts. Escape accepts and returns to the level selector. An empty name becomes `ANON`. Space inserts a space rather than confirming. Developer level/ending auditions do not qualify.

Normal records are never copied into or overwritten by Arcade records. Normal record updates and disk-save snapshots are suppressed while Arcade is active, including any pending normal-save request. Switching off Arcade restores the existing normal scores and unlock checks; any previously pending normal save remains pending.

Arcade treats levels as already completed for completion announcements/endings, matching the PC's completion flags. It does not change the level schedules or bonus progression.

## Memory and validation

The fixed native image arena grows from 221,184 to 225,280 bytes by reducing the adjacent cue region from 16 KiB to 12 KiB. The largest cue file is 11,441 bytes. The cue offset moves to 290,816; the heap stays at 303,104. Total Chip allocation remains 385,024 bytes. Both ADF and WHDLoad builders enforce the new image/cue bounds.

Tests cover ranking/ties/eviction, per-level isolation, ten-character input, deletion, empty names, actual menu/death transitions, prevention of duplicate submissions, normal-save isolation, and existing menu rendering/cache behaviour. Arcade changes are not yet hardware-playtested.

Validation for this build: `make test`, `make test-arcade test-game-loading test-menu-cache test-level-menu-text`, and the ADF/WHDLoad distribution build passed. Copperline A500 OCS (512 KiB Chip + 512 KiB Slow) booted the release ADF and exercised Options, Arcade enable, an actual death, typing a name containing a space and digit, confirmation, the populated table, Hyper Hexagoner with grey text, and switching Arcade off to restore its locked state. Captures are under `scratchpad/arcade/`. The ADF inside `out/Hexagon.zip` is byte-identical to that emulator-tested image. WHDLoad was built but this Arcade flow was not run under WHDLoad.

The Arcade Hyper Hexagoner and Hyper Hexagonest level selectors use a display-only dark navy background (`0x012`), muted blue spokes (`0x246`) and white text (`0xfff`). This keeps both background colours distinct from score text. The normal selector and gameplay retain their original palettes. The override's scope was checked across all 120 combinations of frontend visibility, page, profile and Arcade state.

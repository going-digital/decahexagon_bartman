# PC start and retry gates

Reference executable SHA-256:
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`.
Capture with `python3 tools/capture_pc_lifecycle.py [owned executable]`.
The executable is loaded into private memory; input polling, graphics, audio,
score APIs and disk saves are stubbed. Expected frame delta is fixed to one.
The first-run tutorial is disabled using the already-seen-tutorial flag.

## Rules implemented

`gameinput` (`0x100052980`) calls `restart` (`0x100029c40`) when confirmation is
accepted at transition extent 320. Restart clears score, hit state and walls,
but leaves both player angle fields and the transition extent intact. Hyper
setup subsequently calls `restarthyper` (`0x100029e00`). `gamelogic`
(`0x100056f50`) then advances the new run on that same tick: score becomes one,
the opening wave is generated and extent decreases from 320 to 300. There is
no added 90-tick ready delay. Selection movement happens before confirmation;
restart does not apply a second gameplay turn on that tick.

During live play, extent decreases by 20 per tick to 40. A fatal collision sets
the death timer to one. Following ticks increment it up to 100 and freeze the
score. When the death timer reaches 60, extent increases by 20 per tick until
320. Input is processed before this progression. Starting from extent 40 at
death, the 72nd subsequent tick reaches extent 320; a held confirmation retries
on tick 73. A tap only on tick 72 cannot retry and is not queued.

`pc_lifecycle` retains the PC's extent solely for these gates. It is not a claim
that the current Amiga zoom matches the PC camera. The Amiga confirmation is
Space/joystick fire; held confirmation can retry as soon as the gate opens.
The menu still accepts the existing fire edge. Escape behavior is unchanged.

## Verification

`make test-lifecycle` compares 1,698 native cases:

- 168 snapshots: all six profiles at seven angles, both fresh menu confirmation
  and result-screen retry, before and after the first logic tick.
- 1,530 subsequent death/retry ticks: all six profiles, 85 ticks each with no
  confirmation, continuous confirmation, or a single early tap on tick 72.
  Compares death timer, transition extent, score and retained player angle.

`tools/build_lifecycle_checks.py` extracts 24 death/retry boundary states into
shared host/68000 checks. Those also verify that starting clears hit/block flags
without resetting either player angle. The native fixtures seed a death at
extent 40 and do not replay the preceding collision. Initial states bypass
pending unlock announcements and the tutorial; those paths remain separate.

## Remaining work

The port still uses an adapted planar camera, palette reset and GAME OVER
banner. PC wall retreat, morph continuation during death, camera/field motion,
white flash, sound effects and music fade, tutorial, unlock announcements and
automatic menu selection remain unfinished. The former placeholder random
shake was removed: it consumed gameplay RNG on death without a verified source
basis. No new death presentation is claimed in its place.

Session records/unlocks remain memory-only. Saving them to disk and scripted
ending behavior remain on the implementation plan.

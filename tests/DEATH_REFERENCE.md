# Ordinary PC death movement and polygon restoration

Reference executable SHA-256:
`91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88`.
`tools/capture_pc_death.py` executes `superhex::gamelogic` at `0x100056f50`,
including `gameclass::updatevisualeffects` at `0x10002adb0`, in private memory.
External input, graphics, audio, score APIs and saves are stubbed. The expected
frame delta and simulation delta are one; the tutorial is disabled.

## Ordered behavior

1. Advance any in-progress polygon morph using the existing verified morph
   implementation. Death does not freeze that interpolation.
2. Advance the death timer and result extent using `pc_lifecycle`. Score stays
   frozen. The normal wave selector does not run.
3. Before death timer 60, the special wall-motion velocity is zero. From timer
   60 onward it is -40, multiplied by five: wall distances increase by 200 per
   tick. Widths and active flags are retained. Distances below one clamp to zero.
   This branch visits all records below the current count, including inactive
   holes; it does not dispatch markers or release their wait flag.
4. Remove inactive trailing records from the count after movement. Their stored
   values then remain untouched on later ticks. The fixtures deliberately hash
   those storage slots as well as live records to catch ordering mistakes.
5. Once result extent reaches 320, Hexagon requests morph state 4 whenever there
   are fewer than six sides and no active morph. The request occurs after the
   morph update, so the next tick begins growth. Repeated requests restore six
   sides without interrupting an existing shrink/grow transition.

After an automatic stage handoff, the continued stage's behavior applies until
an incoming extent of 320 would grow beyond 320. At that point the PC clears its
continuation flags before stage-specific death logic. The port applies the
originally selected stage's restoration rule from that tick onward. In
particular, a Hexagon run restores its missing sides even after continuing into
Hexagoner or Hexagonest. New runs reset the progression state normally.

`game.c` calls this path in both death and results states, projecting the updated
morph afterward. `render.c` retains the walls during results so they retreat by
simulation movement rather than disappearing solely because the mode changed.

## Evidence and coverage

`make test-death` compares 17,280 executed-PC ticks: 120 ticks for each of two
starting death timers (1 and 59), six morph states/trajectories, and twelve
profiles/continuation setups. The latter cover all six direct profiles and
normal/hyper runs after either Hexagon handoff or the Hexagoner handoff.

Checks include ordered wall-record hashes, count, integer projected polygon
arc, side count, morph state, marker wait, score, death timer and result extent.
Walls include zero/negative distances, zero widths, inactive holes and tails,
and markers 20/21/22. Records are synthetic; no claim of exhaustive natural
reachability is made for every combination. `tools/build_death_checks.py`
extracts 181 boundary cases for the shared host/68000 self-test.

Reproduce the fixture with `python3 tools/capture_pc_death.py [owned executable]`.
The capture checks the fingerprint and leaves the executable unchanged.

## Remaining limits

These cases begin after collision has already set death timer one, or just
before the retreat boundary. Earlier collision and start/retry behavior have
separate tests. Camera tilt, field rotation, pulse, white flash, sound/music
fade, tutorial and scripted ending remain outside this death implementation.
The result banner and planar rendering are still Amiga adaptations. Unlock
announcements, saved records/unlocks and full transition presentation remain
on the plan.

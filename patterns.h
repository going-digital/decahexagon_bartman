#pragma once

#include <exec/types.h>

// Wall-pattern sequencer. Owns the library of patterns and picks/runs them
// during MODE_PLAYING; spawns walls through game_spawn_wall().

void patterns_reset(void);  // start of a run
void patterns_tick(void);   // once per PLAYING tick

// Count of completed patterns this run - also used by game.c to time
// milestone-triggered field-rotation kicks (see FLICK_TICKS's neighbourhood
// in game.c), matching the source PC game's wave-count-keyed pacing.
UWORD patterns_waves_spawned(void);

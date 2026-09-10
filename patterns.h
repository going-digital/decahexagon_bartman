#pragma once

// Wall-pattern sequencer. Owns the library of patterns and picks/runs them
// during MODE_PLAYING; spawns walls through game_spawn_wall().

void patterns_reset(void);  // start of a run
void patterns_tick(void);   // once per PLAYING tick

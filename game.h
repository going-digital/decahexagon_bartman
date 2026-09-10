#pragma once

#include <exec/types.h>
#include "input.h"

typedef struct sGameState {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UWORD segment_angle_target;
    UWORD player_angle;
    UWORD wall_fraction;
    UWORD draw_distance;
    UWORD draw_distance_target;
    UWORD time_seconds;
    UWORD time_subsecond_frames;
    UWORD record_seconds;
    UWORD record_subsecond_frames;
} GameState;

extern GameState gamestate;

// High-level flow. Rendering branches on this; Phase 1 fills in the gameplay.
typedef enum {
    MODE_ATTRACT,   // title / idle, field drifts
    MODE_READY,     // "BEGIN" lead-in
    MODE_PLAYING,   // live run
    MODE_DEAD,      // hit: brief freeze / shake
    MODE_GAMEOVER,  // results, waiting for fire
} GameMode;

void game_init(void);
void game_update(const InputState* in);  // advance one logic tick
GameMode game_mode(void);

#pragma once

#include <exec/types.h>
#include "input.h"
#include "config.h"

// ---- radial geometry (screen pixels from centre) -----------------------
// Shared by the game logic and the renderer so collision matches what's drawn.
#define HUB_RADIUS       18
#define PLAYER_RADIUS    27   // player sits just outside the hub
#define WALL_THICKNESS   13
#define WALL_SPAWN_DIST  150  // walls appear here and close inward

#define MAX_WALLS        32   // ~2 full rings of NUM_SIDES-1 walls, plus slack

typedef struct sWall {
    UBYTE active;
    UBYTE slot;   // 0..NUM_SIDES-1
    WORD  dist;   // radius of the wall's inner edge
} Wall;

typedef struct sGameState {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UWORD segment_angle_target;
    UWORD player_angle;          // field-relative, 0..65535 around the ring
    UWORD wall_fraction;
    UWORD draw_distance;
    UWORD draw_distance_target;
    UWORD time_seconds;
    UWORD time_subsecond_frames;
    UWORD record_seconds;
    UWORD record_subsecond_frames;
} GameState;

extern GameState gamestate;
extern Wall walls[MAX_WALLS];    // read by the renderer

// High-level flow. Rendering branches on this.
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
UWORD    game_mode_timer(void);          // ticks elapsed in the current mode
WORD     game_shake_x(void);             // camera offset (non-zero during DEAD)
WORD     game_shake_y(void);

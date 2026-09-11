#pragma once

#include <exec/types.h>
#include "input.h"
#include "config.h"

// ---- radial geometry (screen pixels from centre) -----------------------
// Shared by the game logic and the renderer so collision matches what's drawn.
#define HUB_RADIUS       18
#define PLAYER_RADIUS    27   // player sits just outside the hub
// Camera zoom target (game.c) - not the tighter (vertical) screen half-extent:
// wall/hub/player rendering all go through the clipped line renderer, which
// handles off-screen coordinates safely, so nothing here needs to guarantee
// staying within the shorter dimension. Only render_spokes()'s raw,
// UNCLIPPED blit_line() call needs its own hard on-screen bound, which it
// has independently (SPOKE_OUTER_MAX in render.c) - so this can target the
// wider extent instead, for a fuller, more zoomed-in field. A wall spawning
// near straight up/down will briefly be off the top/bottom edge before
// curving into view - expected and safely clipped, not a bug.
#define SCREEN_EDGE_RADIUS (SCREEN_WIDTH / 2 - 10)

// wall_thickness/wall_spawn_dist (in GameState below) are expressed in TIME,
// not fixed distance: thickness ~100ms of travel, spawn dist ~1s of travel,
// at the CURRENT wall_speed - see game.c's update_difficulty(). That keeps
// the visual read (how much empty gap surrounds a wall, how long you get to
// react) constant as wall_speed ramps up through a run, instead of walls
// visually thickening relative to their spacing as they speed up.

#define MAX_WALLS        32   // ~2 full rings of MAX_NUM_SIDES-1 walls, plus slack

typedef struct sWall {
    UBYTE active;
    UBYTE slot;   // 0..gamestate.num_sides-1 (whatever it was when spawned)
    WORD  dist;   // radius of the wall's inner edge
} Wall;

typedef struct sGameState {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UWORD segment_angle_target;
    UBYTE num_sides;              // current field side count - morphs down as a run's difficulty ramps, see game.c's level table
    WORD  wall_thickness;         // ~100ms of travel at the current wall_speed
    WORD  wall_spawn_dist;        // ~1s of travel at the current wall_speed (+ HUB_RADIUS)
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

// For the pattern sequencer (patterns.c):
UWORD game_rng(void);                        // shared 16-bit PRNG
void  game_spawn_wall(UBYTE slot, WORD dist); // add a wall; no-op if walls[] is full
UBYTE game_slot_blocked(UBYTE slot);          // does any in-flight wall (any distance) occupy this slot?

GameMode game_mode(void);
UWORD    game_mode_timer(void);          // ticks elapsed in the current mode
WORD     game_shake_x(void);             // camera offset (non-zero during DEAD)
WORD     game_shake_y(void);
UBYTE    game_on_beat(void);             // 1 on the tick a (placeholder) beat lands
UBYTE    game_new_record(void);          // this run beat the previous best (latches for the GAMEOVER screen)

// Camera zoom, Q8 fixed point (ZOOM_ONE = 1.0). Renderer scales every radius
// by gamestate.draw_distance; it breathes on the beat and drifts with difficulty.
#define ZOOM_ONE 256

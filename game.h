#pragma once
#include <exec/types.h>
#include "input.h"
#include "config.h"
#include "pc_world.h"
#include "pc_palette.h"

/* Temporary 2D renderer coordinates. PC walls project at hub+distance/5;
 * screen scaling is independent of their simulation speed/spawn distance. */
#define HUB_RADIUS 40
#define PLAYER_RADIUS 54
#define ZOOM_ONE 256

typedef struct {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UBYTE num_sides;
    UWORD player_angle;
    UWORD draw_distance,draw_distance_target;
    UWORD time_seconds,time_subsecond_frames;
    UWORD record_seconds,record_subsecond_frames;
} GameState;
extern GameState gamestate;
extern PcWorld game_world;
extern PcPalette game_palette;

typedef enum { MODE_ATTRACT,MODE_READY,MODE_PLAYING,MODE_DEAD,MODE_GAMEOVER } GameMode;
void game_init(void);
void game_update(const InputState *in);
UWORD game_rng(void);
GameMode game_mode(void);
UWORD game_mode_timer(void);
WORD game_shake_x(void);
WORD game_shake_y(void);
UBYTE game_on_beat(void);
UBYTE game_new_record(void);

#if PC_CORE_SELFTEST
UWORD game_live_failure(void);
#endif

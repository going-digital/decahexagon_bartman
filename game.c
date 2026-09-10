#include "game.h"
#include "config.h"

GameState gamestate = {
    .field_angle = 0,
    .field_rotation = 65536 / FRAME_RATE * 3 / 6, // 1 degree per 60Hz frame
    .segment_angle = ((65536 + NUM_SIDES - 1) / NUM_SIDES), // Ensure overflow after last segment
    .segment_angle_target = ((65536 + NUM_SIDES - 1) / NUM_SIDES),
    .player_angle = 0,
    .wall_fraction = 0,
    .draw_distance = 0x500,
    .draw_distance_target = 0x500,
    .time_seconds = 16,
    .time_subsecond_frames = 10,
    .record_seconds = 126,
    .record_subsecond_frames = 10
};

// Ticks are logic frames. TODO: on NTSC the loop runs at 60Hz, so a "second"
// here is 0.83s wall-clock - Phase 6 normalises difficulty to real time.
#define READY_TICKS (FRAME_RATE * 3 / 2) // ~1.5s "BEGIN" lead-in
#define DEAD_TICKS  (FRAME_RATE)         // ~1s hit freeze

static GameMode mode;
static UWORD mode_timer; // ticks elapsed in the current mode

static void set_mode(GameMode m) {
    mode = m;
    mode_timer = 0;
}

GameMode game_mode(void) {
    return mode;
}

static void reset_run(void) {
    gamestate.field_angle = 0;
    gamestate.player_angle = 0;
    gamestate.wall_fraction = 0;
    gamestate.segment_angle = gamestate.segment_angle_target;
    gamestate.draw_distance = gamestate.draw_distance_target;
    gamestate.time_seconds = 0;
    gamestate.time_subsecond_frames = 0;
}

void game_init(void) {
    reset_run();
    set_mode(MODE_ATTRACT);
}

void game_update(const InputState* in) {
    mode_timer++;

    switch (mode) {
    case MODE_ATTRACT:
        if (in->fire_edge) {
            reset_run();
            set_mode(MODE_READY);
        }
        break;

    case MODE_READY:
        if (mode_timer >= READY_TICKS) set_mode(MODE_PLAYING);
        break;

    case MODE_PLAYING:
        if (++gamestate.time_subsecond_frames >= FRAME_RATE) {
            gamestate.time_subsecond_frames = 0;
            gamestate.time_seconds++;
        }
        // TODO Phase 1: player_angle from in->turn, wall spawn/advance, collision.
        break;

    case MODE_DEAD:
        if (mode_timer >= DEAD_TICKS) set_mode(MODE_GAMEOVER);
        break;

    case MODE_GAMEOVER:
        if (in->fire_edge) {
            reset_run();
            set_mode(MODE_READY);
        }
        break;
    }

    // The field never stops turning, in any mode.
    gamestate.field_angle += gamestate.field_rotation;
}

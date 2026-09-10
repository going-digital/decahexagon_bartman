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

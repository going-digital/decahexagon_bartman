#include "pc_core.h"

uint32_t pc_clock_advance(PcClock *clock, uint16_t display_frames, uint16_t display_hz) {
    uint32_t elapsed = (uint32_t)display_frames * PC_TICK_RATE + clock->remainder;
    uint32_t ticks = elapsed / display_hz;
    clock->remainder = (uint16_t)(elapsed % display_hz);
    return ticks;
}

int16_t pc_turn(int16_t angle, uint8_t held, uint8_t degrees_per_tick) {
    if (held & PC_INPUT_POSITIVE) angle += degrees_per_tick;
    else if (held & PC_INPUT_NEGATIVE) angle -= degrees_per_tick;
    if (angle < 0) angle += 360;
    if (angle >= 360) angle -= 360;
    return angle;
}

uint16_t pc_render_angle(int16_t degrees) {
    return (uint16_t)(((uint32_t)(uint16_t)degrees * 65536u) / 360u);
}

void pc_collide(PcPlayer *player, const PcWall *walls, uint16_t count,
                uint8_t sides, int16_t speed) {
    int16_t arc = 360 / sides;
    int16_t slot = player->angle / arc;
    player->blocked = 0;
    for (uint16_t i = 0; i < count; ++i) {
        const PcWall *w = &walls[i];
        if (!w->active || w->slot != slot || w->distance >= 151) continue;
        if (w->distance >= 145 - speed) player->hit = 1;
        else if (w->width > 199) {
            player->angle = player->previous_angle;
            slot = player->angle / arc;
            player->blocked = 1;
        }
    }
    player->previous_angle = player->angle;
}

void pc_move_wall(PcWall *wall, int16_t speed) {
    if (!wall->active) return;
    wall->distance -= speed;
    if (wall->distance < 1) {
        wall->distance = 0;
        wall->width -= speed;
        if (wall->width <= 0) wall->active = 0;
    }
}

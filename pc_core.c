#include "pc_core.h"
#ifdef __m68k__
#include "support/gcc8_c_support.h"
#endif

uint32_t pc_clock_advance(PcClock *clock, uint16_t display_frames, uint16_t display_hz) {
    /* NTSC: (frames*60 + remainder)/60 is exactly frames. */
    if (display_hz == 60) return display_frames;

    /* PAL: frames*60/50 = frames + frames/5. The reciprocal is
     * exact for all uint16 frame counts and needs only a word multiply. */
#ifdef __m68k__
    uint32_t fifths = muluw(display_frames,52429u) >> 18;
#else
    uint32_t fifths = ((uint32_t)display_frames * 52429u) >> 18;
#endif
    uint16_t remainder = (uint16_t)((display_frames-fifths*5u)*10u +
                                    clock->remainder);
    uint32_t ticks = (uint32_t)display_frames + fifths;
    if (remainder >= 50) { remainder -= 50; ++ticks; }
    clock->remainder = remainder;
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
    /* 65536/360 = 182 + 2/45. For 0..359 the reciprocal term
     * floor(degrees*23302/2^19) is exactly floor(degrees*2/45).
     * Both products fit the 68000's unsigned 16x16 multiply. */
    if ((uint16_t)degrees < 360u)
        return (uint16_t)((uint32_t)(uint16_t)degrees * 182u +
                         (((uint32_t)(uint16_t)degrees * 23302u) >> 19));
    /* Preserve the original conversion for non-normalised callers. */
    return (uint16_t)(((uint32_t)(uint16_t)degrees * 65536u) / 360u);
}

static int16_t collision_slot(int16_t angle,int16_t arc,uint16_t reciprocal) {
    if (reciprocal && (uint16_t)angle<360u) {
#ifdef __m68k__
        return (int16_t)(muluw((uint16_t)angle,reciprocal)>>16);
#else
        return (int16_t)(((uint32_t)(uint16_t)angle*reciprocal)>>16);
#endif
    }
    return angle/arc;
}

void pc_collide(PcPlayer *player, const PcWall *walls, uint16_t count,
                uint8_t sides, int16_t speed) {
    static const uint16_t arcs[]={120,90,72,60};
    /* ceil(65536/arc), exact for normalised angles 0..359. */
    static const uint16_t reciprocals[]={547,729,911,1093};
    unsigned regular=sides>=3 && sides<=6;
    int16_t arc = regular ? arcs[sides-3] : 360/sides;
    uint16_t reciprocal = regular ? reciprocals[sides-3] : 0;
    int16_t slot = collision_slot(player->angle,arc,reciprocal);
    player->blocked = 0;
    for (uint16_t i = 0; i < count; ++i) {
        const PcWall *w = &walls[i];
        if (!w->active || w->slot != slot || w->distance >= 151) continue;
        if (w->distance >= 145 - speed) player->hit = 1;
        else if (w->width > 199) {
            player->angle = player->previous_angle;
            slot = collision_slot(player->angle,arc,reciprocal);
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

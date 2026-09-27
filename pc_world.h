#pragma once
#include "pc_core.h"

typedef struct {
    PcWall walls[PC_WALL_CAPACITY];
    uint16_t count; /* source high-water index, not active count */
    uint16_t speed;
    uint16_t delay_ticks; /* ceil(source float delay) at dt=1 */
    uint32_t delay_numerator; /* base_delay *20, denominator speed */
    int16_t spawn_base;
    uint8_t marker_wait;
    uint8_t morph_state;
    uint8_t camera_trigger;
    uint8_t overflow;
    /* Wall movement freeze, refreshed to 20 by pc_morph_tick whenever it
     * processes morph_state 1/2/3 (source this+0x54c0, dt=1 ticks). Decays
     * by one otherwise; movement resumes once it reaches zero. Each frozen
     * movement tick also adds one to the wave delay before scheduling. Source death
     * regrowth (states 4/5) does not refresh it, matching the reference. */
    uint8_t freeze;
} PcWorld;

typedef uint16_t (*PcRandom)(void *context, uint16_t exclusive_bound);
void pc_world_reset(PcWorld *world);
int pc_world_add(PcWorld *world, uint8_t slot, int32_t distance, int32_t width);
void pc_world_move(PcWorld *world);
/* Unknown IDs do not invent geometry. Returns 0; caller preserves source delay. */
int pc_generate_wave(PcWorld *world, uint16_t wave, PcRandom random, void *context);

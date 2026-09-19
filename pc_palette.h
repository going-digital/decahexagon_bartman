#pragma once
#include "pc_core.h"

/* Two-colour projection of PC palette slots 0 (background) and 2 (walls).
 * RGB remains 8-bit through interpolation; only output is quantized to OCS. */
typedef struct {
    uint8_t stage, direction, change;
    uint16_t scheme, target;
    int16_t blend;
    uint8_t start[6], end[6], rgb[6];
} PcPalette;
void pc_palette_reset(PcPalette *p, uint8_t stage);
void pc_palette_start(PcPalette *p,uint8_t stage,uint8_t hyper);
/* One dt=1 reference tick. score is the already-incremented 60 Hz run time;
 * pass zero outside gameplay to cycle colours without stage milestones. */
/* Direct stage/hyper palette selection; preserves current blend and RGB. */
void pc_palette_enter(PcPalette *p,uint8_t stage,uint8_t hyper);
void pc_palette_tick(PcPalette *p, uint32_t score);
uint16_t pc_palette_colour(const PcPalette *p, uint8_t foreground);

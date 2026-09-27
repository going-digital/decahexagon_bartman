#pragma once
#include "pc_world.h"
/* Uninterrupted dt=1 morph trajectories, including binary64's extra endpoint
 * tick. Markers in ordinary normal-Hexagon play are spaced beyond a morph. */
typedef struct { uint8_t sides,phase,growing; } PcMorph;
void pc_morph_reset(PcMorph *m);
/* Also refreshes/decays w->freeze (see pc_world.h); pc_world_move stalls
 * walls while an ordinary shrink/grow is in progress and briefly after. */
void pc_morph_tick(PcMorph *m,PcWorld *w);
uint16_t pc_morph_arc(const PcMorph *m);

#pragma once
#include "pc_core.h"
/* Nominal dt=1 in-plane flip from PC updatevisualeffects. Half-degree output
 * preserves 0.5-degree steps without floating point. Ordinary field rotation
 * is caller-owned when phase==0; do not add it during an active flip. */
typedef struct { uint8_t phase,timer; } PcEndingFlip;
void pc_ending_flip_request(PcEndingFlip *flip);
/* Call during visual-effects update, before the ending phase controller.
 * Requests emitted later by that controller begin on the following tick. */
int16_t pc_ending_flip_tick(PcEndingFlip *flip,unsigned wave_mode);

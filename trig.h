#pragma once

// Camera-scaled Q14 direction tables and polar helpers.

#include <exec/types.h>
#include "support/gcc8_c_support.h"
#include "config.h"

extern WORD sin_table[1024];
extern WORD cos_table[1024];

// Per-frame sin/cos tables, one entry per polygon side.
extern WORD frame_sin[MAX_NUM_SIDES];
extern WORD frame_cos[MAX_NUM_SIDES];

void init_tables(void);
void polar_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y);

// Uses cached, camera-scaled Q14 directions with identical rounding.
void direction_to_cartesian(WORD sine, WORD cosine, UWORD length, WORD* x, WORD* y);

#pragma once
#include <exec/types.h>
/* Normal Hexagon / Hexagoner scheduler. Generator, delay and walls use PC units. */
void patterns_reset(void);
void patterns_tick(void);
UWORD patterns_wave_count(void);
UBYTE patterns_rotation_mode(void);

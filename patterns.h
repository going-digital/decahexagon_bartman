#pragma once
#include <exec/types.h>
#include "pc_core.h"
/* Normal/hyper Hexagon / Hexagoner / Hexagonest scheduler. Generator, delay and walls use PC units. */
void patterns_reset(void);
void patterns_tick(void);
UWORD patterns_wave_count(void);
UBYTE patterns_rotation_mode(void);

UBYTE patterns_stage(void);
UBYTE patterns_transitioned(void);
UBYTE patterns_turn_rate(void);
uint32_t patterns_effective_score(uint32_t elapsed);

UBYTE patterns_rotation_cue(void);
/* One-tick event at the stage-2 effective-score >7200 transition. */
UBYTE patterns_bonus_started(void);

/* Debug entry snapshot; starts just before stage 2 late-phase transition. */
void patterns_test_bonus(void);

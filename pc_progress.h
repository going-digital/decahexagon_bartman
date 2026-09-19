#pragma once
#include "pc_schedule.h"

/* Selected profile stays fixed for score/records; the active selector can
 * advance through the two verified normal/hyper stage handoffs. */
typedef struct {
    PcSchedule schedule;
    uint8_t selected_stage, hyper, handoffs, transitioned;
} PcProgress;
void pc_progress_reset(PcProgress *p,PcWorld *w,uint8_t stage,uint8_t hyper,
                       uint8_t old_rotation,PcRandom random,void *context);
uint32_t pc_progress_score(const PcProgress *p,uint32_t elapsed);
void pc_progress_tick(PcProgress *p,PcWorld *w,uint8_t sides,uint32_t elapsed,
                      PcRandom random,void *context);

#pragma once
#include "pc_world.h"

/* Normal Hexagon / Hexagoner, integer-speed dt=1. Score is the already incremented tick
 * counter. Progression to another stage is outside this selector. */
typedef struct {
    uint32_t wave;
    uint8_t stage; /* 0=Hexagon, 1=Hexagoner; hyper-entry state is not implemented. */
    uint16_t shape_counter;
    int16_t chosen; /* -1 means no generatewave call on this tick */
    uint8_t rotation_mode,rotation_cue,tilt_request;
} PcSchedule;
void pc_schedule_reset(PcSchedule *schedule);
void pc_schedule_tick(PcSchedule *schedule,PcWorld *world,uint8_t sides,
                      uint32_t score,PcRandom random,void *context);

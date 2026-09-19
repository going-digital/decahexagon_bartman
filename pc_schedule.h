#pragma once
#include "pc_world.h"

/* Normal Hexagon / Hexagoner / Hexagonest, integer-speed dt=1. Score is the already incremented tick
 * counter. Progression to another stage is outside this selector. */
typedef struct {
    uint32_t wave;
    uint8_t stage; /* 0=Hexagon, 1=Hexagoner, 2=Hexagonest; hyper-entry state is not implemented. */
    uint16_t shape_counter;
    int16_t chosen; /* -1 means no generatewave call on this tick */
    uint8_t rotation_mode,rotation_cue,tilt_request;
    uint8_t late_phase; /* Stage2 post-7200 wall-clear/rotation event consumed. */
    uint8_t flip_request; /* Stage2 S+0x54b0 event; renderer integration pending. */
} PcSchedule;
void pc_schedule_reset(PcSchedule *schedule);
void pc_schedule_tick(PcSchedule *schedule,PcWorld *world,uint8_t sides,
                      uint32_t score,PcRandom random,void *context);

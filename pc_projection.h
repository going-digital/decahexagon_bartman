#pragma once
#include "pc_world.h"
/* Render-only radial unions. Never sort/merge the simulation's wall records. */
typedef struct { int16_t inner,outer; uint16_t slot; } PcSpan;
uint16_t pc_project_spans(const PcWorld *world,uint8_t sides,PcSpan spans[PC_WALL_CAPACITY]);

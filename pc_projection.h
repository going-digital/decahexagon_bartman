#pragma once
#include "pc_world.h"
/* Render-only radial unions. Never sort/merge the simulation's wall records. */
typedef struct { int16_t inner,outer; uint16_t slot; } PcSpan;
uint16_t pc_project_spans(const PcWorld *world,uint8_t sides,PcSpan spans[PC_WALL_CAPACITY]);
#define PC_SPAN_PREV 1u
#define PC_SPAN_NEXT 2u
/* Input is the sorted, disjoint output of pc_project_spans; sides is 3..6. */
void pc_span_shared_edges(const PcSpan *spans,uint16_t count,uint8_t sides,uint8_t *shared);

/* Outward ending presentation; simulation/marker coordinates stay unchanged. */
uint16_t pc_project_ending_spans(const PcWorld *world,uint8_t sides,PcSpan spans[PC_WALL_CAPACITY]);

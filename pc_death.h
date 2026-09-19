#pragma once
#include "pc_lifecycle.h"
#include "pc_morph.h"
/* Ordinary post-collision tick, nominal dt=1; no tutorial/ending branches. */
void pc_death_tick(PcLifecycle *life,PcWorld *world,PcMorph *morph,uint8_t active_stage,uint8_t selected_stage);

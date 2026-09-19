#pragma once
#include "pc_core.h"
/* Nominal dt=1 lifecycle gates. Extent mirrors PC S+0x19c, not pixel zoom. */
typedef struct { uint32_t elapsed; uint16_t extent; uint8_t death; } PcLifecycle;
void pc_lifecycle_init(PcLifecycle *p);
void pc_lifecycle_start(PcLifecycle *p,PcPlayer *player);
void pc_lifecycle_tick(PcLifecycle *p);
void pc_lifecycle_die(PcLifecycle *p);
uint8_t pc_lifecycle_can_start(const PcLifecycle *p);

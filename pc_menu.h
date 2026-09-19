#pragma once
#include "pc_core.h"
/* Profile index = normal stage + 3 * hyper. Session-only storage. */
typedef struct {
    uint32_t best[6];
    uint8_t completed[6];
} PcRecords;
typedef struct {
    int16_t angle, motion;
    uint8_t wedge, cooldown;
} PcMenu;
void pc_menu_reset(PcMenu *m,uint8_t profile);
void pc_menu_tick(PcMenu *m,uint8_t held);
uint8_t pc_menu_profile(const PcMenu *m);
uint8_t pc_profile_unlocked(const PcRecords *r,uint8_t profile);
uint8_t pc_record_tick(PcRecords *r,uint8_t profile,uint32_t elapsed);

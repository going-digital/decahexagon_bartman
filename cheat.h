#pragma once
#include "config.h"
#if CHEAT_MODE
#include "pc_world.h"
void cheat_reset(void);
/* Returns ordinary input bits. Does not mutate the player or world. */
uint8_t cheat_steer(const PcPlayer *player,const PcWorld *world,
                    uint8_t sides,uint8_t turn_rate);
#endif

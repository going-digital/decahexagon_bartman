#pragma once
#include "pc_core.h"
#ifdef __m68k__
#include "support/gcc8_c_support.h"
#endif
/* Exact for the common uint16 tick range; preserve full-width saved scores. */
static inline uint32_t pc_tick_seconds(uint32_t ticks) {
    if (ticks<=65535u) {
#ifdef __m68k__
        return muluw((uint16_t)ticks,34953u)>>21;
#else
        return ((uint32_t)(uint16_t)ticks*34953u)>>21;
#endif
    }
    return ticks/60u;
}

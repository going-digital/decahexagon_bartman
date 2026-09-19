#pragma once
#include "pc_sfx.h"
#include "config.h"
#if SOUND_EFFECTS
void sfx_init(void);
void sfx_emit(uint16_t clips);
void sfx_shutdown(void);
void sfx_status(unsigned *begun,unsigned *retired,unsigned *active);
#else
static inline void sfx_init(void) {}
static inline void sfx_emit(uint16_t clips) {(void)clips;}
static inline void sfx_shutdown(void) {}
static inline void sfx_status(unsigned *b,unsigned *r,unsigned *a) {*b=*r=*a=0;}
#endif

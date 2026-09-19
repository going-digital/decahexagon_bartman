#pragma once
#include "pc_core.h"
/* One nominal 60 Hz tick, matching gamelogic's integer cue and dt=1. */
uint16_t pc_pulse_tick(uint16_t envelope,int cue,unsigned stage);
/* PCM position -> original Courtesy cue, compensating the MP3's 51 ms lead-in. */
unsigned pc_pulse_cue_index(uint32_t sample);
/* Position inside the current 512-byte DMA block; clamp if an IRQ is delayed. */
uint32_t pc_pcm_position(uint32_t block,uint32_t length,unsigned lines,unsigned period);

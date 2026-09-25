#pragma once
#include <stdint.h>
#include "pc_ending.h"
/* Original camera control at nominal dt=1, with integral degree state.
 * These are control values; projection into screen coordinates is separate. */
int16_t pc_ending_seek_angle(int16_t angle,int16_t target);
int16_t pc_ending_otis_angle(int16_t angle,unsigned wave_mode);
int16_t pc_ending_null_angle(int16_t angle);
typedef struct { int16_t tilt,otis; } PcEndingCamera;
/* Call before pc_ending_step/tick: the PC uses the incoming phase and wave
 * mode, including on a transition tick. Holds suppress camera calls too.
 * Entry angles must come from the live camera, not a forced reset. */
void pc_ending_camera_reference_tick(PcEndingCamera *camera,const PcEnding *ending);
/* Amiga presentation policy: always flat, including phase holds. */
void pc_ending_camera_tick(PcEndingCamera *camera,const PcEnding *ending);

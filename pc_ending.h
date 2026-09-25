#pragma once
#include <stdint.h>
#include "pc_world.h"
/* Non-audio secret-ending phase controller. One call per nominal 60 Hz tick.
 * Palette/flip acknowledgements come from the renderer; these can hold phases.
 * Outputs describe PC requests, not an approximation of the ending walls. */
enum { PC_END_PALETTE=1, PC_END_FLIP=2, PC_END_STOP_MUSIC=4,
       PC_END_FINISH=8, PC_END_RANDOM_ROTATION=16 };
typedef struct {
    uint32_t ticks;
    uint16_t palette;
    uint8_t phase, wait, speed, wave_mode, rotation, camera_target;
    uint8_t scene_mode, hyper;
} PcEnding;
void pc_ending_init(PcEnding *s);
/* elapsed_ms allows reference boundary tests and eventual audio-clock sync.
 * The caller must supply monotonic time; no record/gameplay clock is advanced. */
unsigned pc_ending_step(PcEnding *s,uint32_t elapsed_ms,
                        unsigned flip_pending,unsigned palette_pending);
unsigned pc_ending_tick(PcEnding *s,unsigned flip_pending,unsigned palette_pending);

/* Select only when the caller's spawn delay/marker gate is open. Score is the
 * frozen gameplay score, not ending elapsed time. Returns -1 for no spawn. */
int pc_ending_wave(uint32_t elapsed_ms,uint32_t score,unsigned sides,
                   unsigned shrinking,unsigned speed,PcRandom random,void *context);

/* Phase updates precede this gate. A marker freezes positive spawn delay;
 * zero delay remains ready even with the marker set, matching the PC. */
enum { PC_END_GATE_WAIT, PC_END_GATE_SPAWN, PC_END_GATE_FINISH };
unsigned pc_ending_gate(PcEnding *s,PcWorld *world,uint32_t elapsed_ms);
/* Call after handling phase events and moving existing walls. Completion is
 * observable as phase 255; returns the selected wave or -1 when gated. */
int pc_ending_spawn(PcEnding *s,PcWorld *world,uint32_t elapsed_ms,
                    uint32_t frozen_score,unsigned sides,PcRandom random,void *context);

/* Evaluate after the death timer/extent update. The PC suppresses this branch
 * while its result-suppression flag is set. Only completion state 3 enters the
 * secret sequence; states 1/2 need their own completion presentation. */
unsigned pc_ending_death_entry(unsigned completion,unsigned death_ticks,
                               unsigned extent,unsigned suppressed);

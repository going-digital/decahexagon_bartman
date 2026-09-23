#pragma once

// Unified player input: keyboard (left/right arrows, space, return, escape),
// joystick in port 1, and mouse buttons in port 0 as a quick fallback.
// Poll once per game tick.
//
// Keyboard is read polled from the CIA-A serial port each poll (no IRQ). The
// hardware only sends one press and one release event per key and gates the
// next event on our handshake, so events are never lost, only delayed by up
// to one frame. Upgrade to a level-2 handler if that latency ever matters.

#include <exec/types.h>
#include "config.h"

typedef struct sInputState {
#if CHEAT_MODE
    UBYTE cheat_held;  // top-row 8, held only; absent from release layout
#endif
    UBYTE held;        // PC_INPUT_POSITIVE / PC_INPUT_NEGATIVE, both preserved
    WORD  turn;        // -1 = anticlockwise (left), +1 = clockwise (right), 0
    UBYTE fire;        // 1 while select/confirm (space, return or joy fire) is held
    UBYTE fire_edge;   // 1 only on the tick select goes 0 -> 1
    UBYTE back_edge;   // 1 only on the tick Escape goes 0 -> 1
} InputState;

void input_init(void);              // arm the pot lines, put keyboard SP in input mode
void input_poll(InputState* in);    // refresh `in` from the hardware

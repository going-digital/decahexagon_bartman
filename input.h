#pragma once

// Unified player input: joystick in port 1, mouse buttons in port 0.
// Poll once per game tick. Keyboard is not handled yet (bare-metal keyboard
// needs the CIA-A serial handshake) - TODO for a later Phase 0 pass.
// (Named InputState, not Input: dos.library already exports Input().)

#include <exec/types.h>

typedef struct sInputState {
    WORD  turn;      // -1 = anticlockwise (left), +1 = clockwise (right), 0 = none
    UBYTE fire;      // 1 while a confirm/fire control is held
    UBYTE fire_edge; // 1 only on the tick fire goes 0 -> 1
    UBYTE quit;      // dev escape: both mouse buttons held
} InputState;

void input_init(void);              // arm the pot lines for button reads
void input_poll(InputState* in);    // refresh `in` from the hardware

#include "input.h"
#include "system.h"

// JOY1DAT direction decode (digital joystick in port 1).
// Left/right come straight from bits 9 and 1; up/down would need the
// bit9^bit8 / bit1^bit0 gray-code XOR, which we don't need here.
#define JOY1_RIGHT(v) ((v) & 0x0002)
#define JOY1_LEFT(v)  ((v) & 0x0200)

static UBYTE prev_fire;

void input_init(void) {
    // Drive the POT lines high so the mouse RMB / joystick 2nd button pull
    // them low when pressed (POTGOR reads in custom->potinp).
    custom->potgo = 0xff00;
    prev_fire = 0;
}

void input_poll(InputState* in) {
    UWORD joy = custom->joy1dat;
    WORD lmb = MouseLeft();
    WORD rmb = MouseRight();

    // Mouse buttons double as the two turn controls for playtesting:
    // RMB = anticlockwise, LMB = clockwise. Both together = quit.
    WORD left  = JOY1_LEFT(joy)  || rmb;
    WORD right = JOY1_RIGHT(joy) || lmb;

    // Opposing directions cancel rather than jitter.
    in->turn = (WORD)(right != 0) - (WORD)(left != 0);

    UBYTE fire = JoyFire() ? 1 : 0;
    in->fire = fire;
    in->fire_edge = (fire && !prev_fire) ? 1 : 0;
    prev_fire = fire;

    in->quit = (lmb && rmb) ? 1 : 0;
}

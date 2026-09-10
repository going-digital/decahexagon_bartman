#include "input.h"
#include "system.h"

// JOY1DAT direction decode (digital joystick in port 1).
#define JOY1_RIGHT(v) ((v) & 0x0002)
#define JOY1_LEFT(v)  ((v) & 0x0200)

// Amiga raw key codes
#define KEY_LEFT   0x4f
#define KEY_RIGHT  0x4e
#define KEY_SPACE  0x40
#define KEY_ESC    0x45

// Held key state, maintained across polls from press/release events.
static UBYTE k_left, k_right, k_space, k_esc;
// Edge flags, set by kbd_scan, consumed (cleared) by input_poll.
static UBYTE k_space_edge, k_esc_edge;

static UBYTE prev_fire;

// Hold the keyboard handshake line low for ~2 scanlines (~130us, over the
// 75us minimum), independent of CPU speed.
static void kbd_delay(void) {
    UBYTE p = custom->vhposr_h;
    for (WORD lines = 0; lines < 2; ) {
        UBYTE n = custom->vhposr_h;
        if (n != p) { p = n; ++lines; }
    }
}

static void kbd_scan(void) {
    // At most 2 key events per poll: the keyboard sends one press + one
    // release per key and gates the next on our handshake, so a real backlog
    // is rare - and this caps the cost if the handshake ever misfires.
    for (WORD guard = 2; guard-- > 0; ) {
        // Reading CIA-A ICR clears its event flags; nothing else here uses
        // them (LSP's CIA interrupt is on CIA-B).
        if (!(ciaa->ciaicr & CIAICRF_SP)) break;

        UBYTE raw = ciaa->ciasdr;

        // Acknowledge: drive SP low as an output for the handshake pulse.
        // Timer A must be stopped while SP is an output, otherwise its
        // underflows shift the SDR out and re-raise the SP flag (phantom
        // "bytes" that spin this loop and eat ~1.5ms/frame).
        UBYTE cra = ciaa->ciacra;
        ciaa->ciacra = (UBYTE)((cra & ~CIACRAF_START) | CIACRAF_SPMODE);
        kbd_delay();
        ciaa->ciacra = (UBYTE)(cra & ~CIACRAF_SPMODE); // SP back to input, restore START

        // Wire byte is bit-inverted; rotate right 1 so bit7 = up/down, 6..0 = code.
        UBYTE n = (UBYTE)~raw;
        UBYTE code = (UBYTE)((n >> 1) | (n << 7));
        UBYTE up = code & 0x80;

        switch (code & 0x7f) {
        case KEY_LEFT:  k_left  = !up; break;
        case KEY_RIGHT: k_right = !up; break;
        case KEY_SPACE: k_space = !up; if (!up) k_space_edge = 1; break;
        case KEY_ESC:   k_esc   = !up; if (!up) k_esc_edge = 1;   break;
        }
    }
}

void input_init(void) {
    // Drive the POT lines high so the mouse RMB / joystick 2nd button pull
    // them low when pressed (POTGOR reads in custom->potinp).
    custom->potgo = 0xff00;
    // Make sure the keyboard serial port is in input mode.
    ciaa->ciacra &= (UBYTE)~CIACRAF_SPMODE;
    prev_fire = 0;
    k_left = k_right = k_space = k_esc = 0;
    k_space_edge = k_esc_edge = 0;
    kbd_scan(); // drain anything already pending
    k_space_edge = k_esc_edge = 0;
}

void input_poll(InputState* in) {
    kbd_scan();

    UWORD joy = custom->joy1dat;
    WORD lmb = MouseLeft();
    WORD rmb = MouseRight();

    // Keyboard arrows, or joystick, or (for quick testing) mouse buttons:
    // RMB = anticlockwise, LMB = clockwise.
    WORD left  = k_left  || JOY1_LEFT(joy)  || rmb;
    WORD right = k_right || JOY1_RIGHT(joy) || lmb;

    // Opposing directions cancel rather than jitter.
    in->turn = (WORD)(right != 0) - (WORD)(left != 0);

    UBYTE fire = (k_space || JoyFire()) ? 1 : 0;
    in->fire = fire;
    in->fire_edge = (k_space_edge || (fire && !prev_fire)) ? 1 : 0;
    prev_fire = fire;

    in->back_edge = k_esc_edge;
    in->quit = (lmb && rmb) ? 1 : 0; // dev hard-exit; Escape is handled per-mode

    k_space_edge = 0;
    k_esc_edge = 0;
}

#pragma once

// System takeover, raster timing, VBR interrupt handling and raw input.

#include "hw.h"
#include <proto/exec.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>

extern struct ExecBase *SysBase;
extern struct DosLibrary *DOSBase;
extern struct GfxBase *GfxBase;

extern volatile short frameCounter;

void TakeSystem(void);
void FreeSystem(void);

void WaitVbl(void);
void WaitLine(USHORT line);

void SetInterruptHandler(APTR interrupt);
APTR GetInterruptHandler(void);

// VBL interrupt handler: services music (ThePlayer builds) and bumps frameCounter.
__attribute__((interrupt)) void interruptHandler(void);

__attribute__((always_inline)) inline void WaitBlt(void) {
    *(volatile UWORD*)&custom->dmaconr;
    while (*(volatile UWORD*)&custom->dmaconr & DMAF_BLTDONE);
}

// Raw button reads. Port 0 = mouse port (left socket), port 1 = joystick port.
// Active-low: pressed => bit clears.
__attribute__((always_inline)) inline short MouseLeft(void) {
    return !(ciaa->ciapra & CIAF_GAMEPORT0);
}

__attribute__((always_inline)) inline short MouseRight(void) {
    return !(custom->potinp & POTINF_L_MOUSE_BUT2);
}

__attribute__((always_inline)) inline short JoyFire(void) {
    return !(ciaa->ciapra & CIAF_GAMEPORT1);
}

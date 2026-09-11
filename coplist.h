#pragma once

// Helpers for hand-building copper lists.

#include "hw.h"
#include "config.h"

// End-of-list copper list, parked in chip mem (defined in coplist.c).
extern const UWORD copper2[2];

__attribute__((always_inline)) inline
USHORT* copWrite(USHORT* copListEnd, UWORD offset, UWORD data) {
    *copListEnd++ = offset;
    *copListEnd++ = data;
    return copListEnd;
}

__attribute__((always_inline)) inline
USHORT* copWritePtr(USHORT* copListEnd, UWORD offset, void* pointer) {
    ULONG ptr = (ULONG)pointer;
    *copListEnd++ = offset;
    *copListEnd++ = (ULONG)ptr >> 16;
    *copListEnd++ = offset + 2;
    *copListEnd++ = (UWORD)ptr;
    return copListEnd;
}

__attribute__((always_inline)) inline
USHORT* copWaitXY(USHORT *copListEnd, USHORT x, USHORT i) {
    return copWrite(copListEnd, (i << 8) | (x << 1) | 1, 0xfffe);
}

__attribute__((always_inline)) inline
USHORT* copWaitY(USHORT* copListEnd, USHORT i) {
    return copWrite(copListEnd, (i << 8) | 4 | 1, 0xfffe);
}

// set up a 320x240 lowres display
__attribute__((always_inline)) inline USHORT* screenScanDefault(USHORT* copListEnd) {
    const USHORT width = SCREEN_WIDTH;
    const USHORT height = SCREEN_HEIGHT;
    const USHORT x = DISPLAY_HW_X;
    const USHORT y = DISPLAY_HW_Y;
    const USHORT RES = 8; //8=lowres,4=hires
    USHORT xstop = x + width;
    USHORT ystop = y + height;
    USHORT fw = (x >> 1) - RES;

    copListEnd = copWrite(copListEnd, offsetof(struct Custom, ddfstrt), fw);
    copListEnd = copWrite(copListEnd, offsetof(struct Custom, ddfstop), fw + (((width >> 4) - 1) << 3));
    copListEnd = copWrite(copListEnd, offsetof(struct Custom, diwstrt), x + (y << 8));
    copListEnd = copWrite(copListEnd, offsetof(struct Custom, diwstop), (xstop - 256) + ((ystop - 256) << 8));
    return copListEnd;
}

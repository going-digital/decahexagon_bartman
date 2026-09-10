#include "system.h"
#include "config.h"
#include "audio.h"
#include <graphics/gfxmacros.h>

struct ExecBase *SysBase;
struct DosLibrary *DOSBase;
struct GfxBase *GfxBase;

struct Custom *custom = (struct Custom*)0xdff000;
struct CIA *ciaa = (struct CIA*)0xbfe001;
struct CIA *ciab = (struct CIA*)0xbfd000;

volatile short frameCounter = 0;

//backup
static UWORD SystemInts;
static UWORD SystemDMA;
static UWORD SystemADKCON;
static volatile APTR VBR = 0;
static APTR SystemIrq;

static struct View *ActiView;

static APTR GetVBR(void) {
    APTR vbr = 0;
    UWORD getvbr[] = { 0x4e7a, 0x0801, 0x4e73 }; // MOVEC.L VBR,D0 RTE

    if (SysBase->AttnFlags & AFF_68010)
        vbr = (APTR)Supervisor((ULONG (*)())getvbr);

    return vbr;
}

void SetInterruptHandler(APTR interrupt) {
    *(volatile APTR*)(((UBYTE*)VBR) + 0x6c) = interrupt;
}

APTR GetInterruptHandler(void) {
    return *(volatile APTR*)(((UBYTE*)VBR) + 0x6c);
}

//vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
//vsync begins at line 2 hpos 132 and ends at vpos 5 hpos 18
void WaitVbl(void) {
    debug_start_idle();
    while (1) {
        ULONG vpos = custom->vposr;
        vpos &= 0x1ff00;
        if (vpos != (311 << 8))
            break;
    }
    while (1) {
        ULONG vpos = *(volatile ULONG*)&custom->vposr;
        vpos &= 0x1ff00;
        if (vpos == (311 << 8))
            break;
    }
    debug_stop_idle();
}

void WaitLine(USHORT line) {
    while (1) {
        ULONG vpos = *(volatile ULONG*)&custom->vposr;
        if (((vpos >> 8) & 511) == line)
                break;
    }
}

void TakeSystem(void) {
    Forbid();
    //Save current interrupts and DMA settings so we can restore them upon exit.
    SystemADKCON = custom->adkconr;
    SystemInts = custom->intenar;
    SystemDMA = custom->dmaconr;
    ActiView = GfxBase->ActiView; //store current view

    LoadView(0);
    WaitTOF();
    WaitTOF();

    WaitVbl();
    WaitVbl();

    OwnBlitter();
    WaitBlit();
    Disable();

    custom->intena = 0x7fff;//disable all interrupts
    custom->intreq = 0x7fff;//Clear any interrupts that were pending

    custom->dmacon = 0x7fff;//Clear all DMA channels

    //set all colors black
    for(int a=0; a < 32; a++)
        custom->color[a]=0;

    WaitVbl();
    WaitVbl();

    VBR = GetVBR();
    SystemIrq = GetInterruptHandler(); // Store interrupt register
}

void FreeSystem(void) {
    WaitVbl();
    WaitBlit();
    custom->intena = 0x7fff; // Disable all interrupts
    custom->intreq = 0x7fff; // Clear any interrupts that were pending
    custom->dmacon = 0x7fff; // Clear all DMA channels

    // Restore interrupts
    SetInterruptHandler(SystemIrq);

    /* Restore system copper list(s). */
    custom->cop1lc = (ULONG)GfxBase->copinit;
    custom->cop2lc = (ULONG)GfxBase->LOFlist;
    custom->copjmp1 = 0x7fff; // Start coppper

    /* Restore all interrupts and DMA settings. */
    custom->intena = SystemInts | INTF_SETCLR;
    custom->dmacon = SystemDMA | DMAF_SETCLR;
    custom->adkcon = SystemADKCON | ADKF_SETCLR;

    WaitBlit();
    DisownBlitter();
    Enable();

    LoadView(ActiView);
    WaitTOF();
    WaitTOF();

    Permit();
}

__attribute__((interrupt)) void interruptHandler(void) {
    custom->intreq = INTF_VERTB;
    custom->intreq = INTF_VERTB; // Reset vbl req. twice for a4000 bug.
#ifdef MUSIC
    // DEMO - ThePlayer
    p61Music();
#endif
    // DEMO - increment frameCounter
    frameCounter++;
}

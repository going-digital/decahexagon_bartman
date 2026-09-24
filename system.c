#include "system.h"
#include "config.h"
#if MUSIC_FIB_STREAM
#include "tests/fib_stream.h"
#endif
#include <graphics/gfxmacros.h>

struct ExecBase *SysBase;
struct DosLibrary *DOSBase;
struct GfxBase *GfxBase;

struct Custom *custom = (struct Custom*)0xdff000;
struct CIA *ciaa = (struct CIA*)0xbfe001;
struct CIA *ciab = (struct CIA*)0xbfd000;

volatile short frameCounter = 0;

#if !TRACKLOADER
//backup
static UWORD SystemInts;
static UWORD SystemDMA;
static UWORD SystemADKCON;
#endif
static volatile APTR VBR = 0;
#if !TRACKLOADER
static APTR SystemIrq;
#endif
static APTR volatile pending_display;

void QueueDisplayList(APTR list) {
    /* Publish only after all Chip RAM list/sprite writes are complete. */
    __asm volatile ("" ::: "memory");
    pending_display = list;
}

void WaitDisplayList(void) {
    while (pending_display) {}
    __asm volatile ("" ::: "memory");
}

#if !TRACKLOADER
static struct View *ActiView;

// Sprite pointers + colour registers (17-31 are the sprite colour banks).
// Unlike the bitplane colours, the OS never re-issues these once it's
// running - the mouse pointer sprite is set up once, not redrawn every
// frame - so anything we leave pointing at our own buffers/colours (the HUD)
// would otherwise linger as garbage after we hand the system back.
static APTR SystemSprPt[8];
static UWORD SystemColor[32];

/* Supervisor callback lives in executable text, never freshly written stack
 * memory: separate instruction/data caches need no runtime synchronization. */
extern ULONG ReadVBRSupervisor(void);

static APTR GetVBR(void) {
    APTR vbr = 0;

    if (SysBase->AttnFlags & AFF_68010)
        vbr = (APTR)Supervisor(ReadVBRSupervisor);

    return vbr;
}

#endif

APTR GetSystemVBR(void) { return (APTR)VBR; }

void SetInterruptHandler(APTR interrupt) {
    *(volatile APTR*)(((UBYTE*)VBR) + 0x6c) = interrupt;
}

APTR GetInterruptHandler(void) {
    return *(volatile APTR*)(((UBYTE*)VBR) + 0x6c);
}

// Wait for the next vertical beam wrap. Both PAL and NTSC reach line256;
// a PAL-only wait for line311 deadlocks before startup on an NTSC machine.
void WaitVbl(void) {
#if BUILD_DEBUG
    debug_start_idle();
#endif
    while ((*(volatile ULONG*)&custom->vposr & 0x1ff00) < (256UL << 8)) {}
    while ((*(volatile ULONG*)&custom->vposr & 0x1ff00) >= (256UL << 8)) {}
#if BUILD_DEBUG
    debug_stop_idle();
#endif
}

void WaitLine(USHORT line) {
    while (1) {
        ULONG vpos = *(volatile ULONG*)&custom->vposr;
        if (((vpos >> 8) & 511) == line)
                break;
    }
}

static void StopFloppyMotors(void) {
    volatile UBYTE *const port = (volatile UBYTE *)0xbfd100;
    // Motor off, select all drives to latch it, then deselect all drives.
    *port |= 0xf8;
    __asm__ volatile ("nop" ::: "memory");
    *port &= 0x87;
    __asm__ volatile ("nop" ::: "memory");
    *port |= 0x78;
    __asm__ volatile ("nop" ::: "memory");
}

#if TRACKLOADER
void TrackSystemSetVBR(void *vbr) { VBR=vbr; }
void TakeSystem(void) {
    __asm volatile("move.w #0x2700,%%sr" : : : "memory","cc");
    custom->intena=0x7fff;custom->intreq=0x7fff;
    custom->dmacon=0x7fff;pending_display=0;frameCounter=0;
    StopFloppyMotors();
    *(volatile UWORD *)0xdff106=0;custom->fmode=0;
    for(unsigned i=0;i<32;i++)custom->color[i]=0;
}
void FreeSystem(void) {
    __asm volatile("move.w #0x2700,%%sr" : : : "memory","cc");
    WaitBlt();
    custom->intena=0x7fff;custom->intreq=0x7fff;
    custom->dmacon=0x7fff;pending_display=0;
}
#else
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

    StopFloppyMotors(); // Direct CIA access with OS interrupts disabled.

    // Use OCS-compatible sprite resolution and DMA fetch widths on AGA.
    *(volatile UWORD *)0xdff106 = 0; // BPLCON3 is absent from the SDK struct.
    custom->fmode = 0;

    //set all colors black, saving them (and the sprite pointers) to restore on exit
    for(int a=0; a < 32; a++) {
        SystemColor[a] = custom->color[a];
        custom->color[a]=0;
    }
    for(int a=0; a < 8; a++)
        SystemSprPt[a] = custom->sprpt[a];

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
    pending_display = 0;

    // Restore interrupts
    SetInterruptHandler(SystemIrq);

    /* Restore system copper list(s). */
    custom->cop1lc = (ULONG)GfxBase->copinit;
    custom->cop2lc = (ULONG)GfxBase->LOFlist;
    custom->copjmp1 = 0x7fff; // Start coppper

    /* Restore sprite pointers and colours (DMA is still off here, so this
       can't race a fetch) before DMA/the OS's view come back. */
    for(int a=0; a < 8; a++)
        custom->sprpt[a] = SystemSprPt[a];
    for(int a=0; a < 32; a++)
        custom->color[a] = SystemColor[a];

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

#endif

__attribute__((interrupt)) void interruptHandler(void) {
    custom->intreq = INTF_VERTB;
    custom->intreq = INTF_VERTB; // Reset vbl req. twice for a4000 bug.
    if (pending_display) {
        /* Keep the beam check and pointer write together even if an audio
         * IRQ arrives. Only this short publication masks higher interrupts.
         * COP1 has restarted into the fixed WAIT; no CPU COPJMP or blitter
         * DMA pause is needed. A late IRQ repeats the previous frame. */
#ifdef __m68k__
        UWORD saved_sr;
        __asm volatile ("move.w %%sr,%0\n\tori.w #0x0700,%%sr"
                        : "=d"(saved_sr) : : "memory", "cc");
#endif
        if (((*(volatile ULONG*)&custom->vposr >> 8) & 511) < 8) {
            custom->cop2lc = (ULONG)pending_display;
            pending_display = 0; // Old list and frame buffers may be reused.
        }
#ifdef __m68k__
        __asm volatile ("move.w %0,%%sr" : : "d"(saved_sr) : "memory", "cc");
#endif
    }
    // DEMO - increment frameCounter
    frameCounter++;
#if MUSIC_FIB_STREAM && !FIB_TRIAL_MAINLOOP
    fib_stream_fill();
#endif
}

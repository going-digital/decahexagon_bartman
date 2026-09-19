#include "system.h"
#include "config.h"
#if MUSIC_FIB_STREAM
#include "tests/fib_stream.h"
#endif
#include <graphics/gfxmacros.h>
#include <devices/trackdisk.h>

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

// Sprite pointers + colour registers (17-31 are the sprite colour banks).
// Unlike the bitplane colours, the OS never re-issues these once it's
// running - the mouse pointer sprite is set up once, not redrawn every
// frame - so anything we leave pointing at our own buffers/colours (the HUD)
// would otherwise linger as garbage after we hand the system back.
static APTR SystemSprPt[8];
static UWORD SystemColor[32];

static APTR GetVBR(void) {
    APTR vbr = 0;
    UWORD getvbr[] = { 0x4e7a, 0x0801, 0x4e73 }; // MOVEC.L VBR,D0 RTE

    if (SysBase->AttnFlags & AFF_68010)
        vbr = (APTR)Supervisor((ULONG (*)())getvbr);

    return vbr;
}

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
    debug_start_idle();
    while ((*(volatile ULONG*)&custom->vposr & 0x1ff00) < (256UL << 8)) {}
    while ((*(volatile ULONG*)&custom->vposr & 0x1ff00) >= (256UL << 8)) {}
    debug_stop_idle();
}

/* Called with interrupts/DMA disabled after takeover. Observe a complete
 * field, rather than relying on the OS default or an ECS-only mode bit. */
void DetectVideoTiming(void) {
    WaitVbl();
    UWORD previous=0,maximum=0;
    for (;;) {
        UWORD line=(*(volatile ULONG*)&custom->vposr>>8)&511;
        if(line<previous) break;
        if(line>maximum) maximum=line;
        previous=line;
    }
    video_select(maximum+1);
}

void WaitLine(USHORT line) {
    while (1) {
        ULONG vpos = *(volatile ULONG*)&custom->vposr;
        if (((vpos >> 8) & 511) == line)
                break;
    }
}

static void StopFloppyMotors(void) {
    /* Do this while trackdisk's task/interrupts can still service DoIO.
     * Unlike a raw CIA write, TD_MOTOR also updates the driver's state so
     * disk access works normally when we return to AmigaOS.
     * Build the reply port locally: CreateMsgPort requires Kickstart 2.0. */
    BYTE signal = AllocSignal(-1);
    if (signal == -1) return;
    struct MsgPort port = {0};
    port.mp_Node.ln_Type = NT_MSGPORT;
    port.mp_Flags = PA_SIGNAL;
    port.mp_SigBit = signal;
    port.mp_SigTask = FindTask(0);
    port.mp_MsgList.lh_Head = (struct Node*)&port.mp_MsgList.lh_Tail;
    port.mp_MsgList.lh_TailPred = (struct Node*)&port.mp_MsgList.lh_Head;

    for (ULONG unit = 0; unit < NUMUNITS; ++unit) {
        struct IOExtTD request = {0};
        struct IOStdReq *io = &request.iotd_Req;
        io->io_Message.mn_Node.ln_Type = NT_MESSAGE;
        io->io_Message.mn_ReplyPort = &port;
        io->io_Message.mn_Length = sizeof(request);
        if (OpenDevice(TD_NAME, unit, (struct IORequest*)io, 0) == 0) {
            io->io_Command = TD_MOTOR;
            io->io_Length = 0; // Motor off; no disk data is modified.
            DoIO((struct IORequest*)io);
            CloseDevice((struct IORequest*)io);
        }
    }
    FreeSignal(signal);
}

void TakeSystem(void) {
    StopFloppyMotors();
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

__attribute__((interrupt)) void interruptHandler(void) {
    custom->intreq = INTF_VERTB;
    custom->intreq = INTF_VERTB; // Reset vbl req. twice for a4000 bug.
    // DEMO - increment frameCounter
    frameCounter++;
#if MUSIC_FIB_STREAM && !FIB_TRIAL_MAINLOOP
    fib_stream_fill();
#endif
}

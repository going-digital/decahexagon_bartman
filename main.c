#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>
#include <graphics/gfxmacros.h>
#include "custom.h"
#include <hardware/custom.h>
#include <hardware/dmabits.h>
#include <hardware/intbits.h>
#include <hardware/cia.h>
#include "paulabits.h"

//config
//#define MUSIC
//#define MUSIC_LSP

#define SCREEN_WIDTH (320)
#define SCREEN_HEIGHT (256)
#define FRAME_RATE (50)
#define SCREEN_WIDTH_BYTES (SCREEN_WIDTH >> 3)

struct ExecBase *SysBase;
struct Custom *custom = (struct Custom*)0xdff000;
struct CIA *ciaa = (struct CIA*)0xbfe001;
struct CIA *ciab = (struct CIA*)0xbfd000;
struct DosLibrary *DOSBase;
struct GfxBase *GfxBase;

//backup
static UWORD SystemInts;
static UWORD SystemDMA;
static UWORD SystemADKCON;
static volatile APTR VBR=0;
static APTR SystemIrq;
 
struct View *ActiView;

#define BITPLANE_SIZE (SCREEN_HEIGHT * SCREEN_WIDTH_BYTES)

UWORD *bitplane_bg1;
UWORD *bitplane_fg1;
UWORD *bitplane_bg2;
UWORD *bitplane_fg2;

typedef struct sGameState {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UWORD segment_angle_target;
    UWORD player_angle;
    UWORD wall_fraction;
    UWORD draw_distance;
    UWORD draw_distance_target;
    UWORD time_seconds;
    UWORD time_subsecond_frames;
    UWORD record_seconds;
    UWORD record_subsecond_frames;
} GameState;

GameState gamestate = {
    .field_angle = 0,
    .field_rotation = 65536 / FRAME_RATE,
    .segment_angle = 65536 / 6,
    .segment_angle_target = 65536 / 6,
    .player_angle = 0,
    .wall_fraction = 0,
    .draw_distance = 0x500,
    .draw_distance_target = 0x500,
    .time_seconds = 16,
    .time_subsecond_frames = 10,
    .record_seconds = 126,
    .record_subsecond_frames = 10
};


#define BLITHOG_ON custom->dmacon = (DMAF_SETCLR | DMAF_BLITHOG)
#define BLITHOG_OFF custom->dmacon = DMAF_BLITHOG

__attribute__((always_inline)) inline void blit_cls(void *bitplane);
void blit_line_or(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);
__attribute__((always_inline)) inline void blit_line(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);
__attribute__((always_inline)) inline void blit_wait();

WORD sin_table[1024];

void init_tables();
__attribute__((always_inline)) inline USHORT* copWrite(USHORT* copListEnd, UWORD offset, UWORD data);
void radial_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y);

static APTR GetVBR(void) {
    APTR vbr = 0;
    UWORD getvbr[] = { 0x4e7a, 0x0801, 0x4e73 }; // MOVEC.L VBR,D0 RTE

    if (SysBase->AttnFlags & AFF_68010) 
        vbr = (APTR)Supervisor((ULONG (*)())getvbr);

    return vbr;
}

void SetInterruptHandler(APTR interrupt) {
    *(volatile APTR*)(((UBYTE*)VBR)+0x6c) = interrupt;
}

APTR GetInterruptHandler() {
    return *(volatile APTR*)(((UBYTE*)VBR)+0x6c);
}

//vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
//vsync begins at line 2 hpos 132 and ends at vpos 5 hpos 18 
void WaitVbl() {
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
        if (vpos == (311<<8))
            break;
    }
    debug_stop_idle();
}

void WaitLine(USHORT line) {
    while (1) {
        ULONG vpos = *(volatile ULONG*)&custom->vposr;
        if(((vpos >> 8) & 511) == line)
                break;
    }
}

__attribute__((always_inline)) inline void WaitBlt() {
    UWORD tst = *(volatile UWORD*)&custom->dmaconr;
    (void)tst;
    while (*(volatile UWORD*)&custom->dmaconr & DMAF_BLTDONE);
}

void TakeSystem() {
    Forbid();
    //Save current interrupts and DMA settings so we can restore them upon exit. 
    SystemADKCON=custom->adkconr;
    SystemInts=custom->intenar;
    SystemDMA=custom->dmaconr;
    ActiView=GfxBase->ActiView; //store current view

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
    SystemIrq = GetInterruptHandler(); //store interrupt register
}

void FreeSystem() { 
    WaitVbl();
    WaitBlit();
    custom->intena = 0x7fff;//disable all interrupts
    custom->intreq = 0x7fff;//Clear any interrupts that were pending
    custom->dmacon = 0x7fff;//Clear all DMA channels

    //restore interrupts
    SetInterruptHandler(SystemIrq);

    /*Restore system copper list(s). */
    custom->cop1lc = (ULONG)GfxBase->copinit;
    custom->cop2lc = (ULONG)GfxBase->LOFlist;
    custom->copjmp1 = 0x7fff; //start coppper

    /*Restore all interrupts and DMA settings. */
    custom->intena = SystemInts|0x8000;
    custom->dmacon = SystemDMA|0x8000;
    custom->adkcon = SystemADKCON|0x8000;

    WaitBlit();	
    DisownBlitter();
    Enable();

    LoadView(ActiView);
    WaitTOF();
    WaitTOF();

    Permit();
}

__attribute__((always_inline)) inline short MouseLeft() {
    return !(ciaa->ciapra & CIAB_GAMEPORT0);
}	


__attribute__((always_inline)) inline short MouseRight(){
    return !(custom->potinp & POTINF_L_MOUSE_BUT2);
    //return !((*(volatile UWORD*)0xdff016)&(1<<10));
}

// DEMO - INCBIN
volatile short frameCounter = 0;
INCBIN(colors, "image.pal")
//INCBIN_CHIP(image, "image.bpl") // load image into chipmem so we can use it without copying
//INCBIN_CHIP(bob, "bob.bpl")

// put copperlist into chip mem so we can use it without copying
const UWORD copper2[] __attribute__((section (".MEMF_CHIP"))) = {
    //offsetof(struct Custom, color[0]), 0x0fff,
    0xffff, 0xfffe // end copper list
};

void* doynaxdepack(const void* input, void* output) { // returns end of output data, input needs to be 16-bit aligned!
    register volatile const void* _a0 ASM("a0") = input;
    register volatile       void* _a1 ASM("a1") = output;
    __asm volatile (
        "movem.l %%d0-%%d7/%%a2-%%a6,-(%%sp)\n"
        "jsr _doynaxdepack_vasm\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a2-%%a6"
    : "+rf"(_a0), "+rf"(_a1)
    :
    : "cc", "memory");
    return (void*)_a1;
}

#ifdef MUSIC
    // Demo - Module Player - ThePlayer 6.1a: https://www.pouet.net/prod.php?which=19922
    // The Player® 6.1A: Copyright © 1992-95 Jarno Paananen
    // P61.testmod - Module by Skylord/Sector 7 
    INCBIN(player, "player610.6.no_cia.bin")
    INCBIN_CHIP(module, "testmod.p61")

    int p61Init(const void* module) { // returns 0 if success, non-zero otherwise
        register volatile const void* _a0 ASM("a0") = module;
        register volatile const void* _a1 ASM("a1") = NULL;
        register volatile const void* _a2 ASM("a2") = NULL;
        register volatile const void* _a3 ASM("a3") = player;
        register                int   _d0 ASM("d0"); // return value
        __asm volatile (
            "movem.l %%d1-%%d7/%%a4-%%a6,-(%%sp)\n"
            "jsr 0(%%a3)\n"
            "movem.l (%%sp)+,%%d1-%%d7/%%a4-%%a6"
            : "=r" (_d0), "+rf"(_a0), "+rf"(_a1), "+rf"(_a2), "+rf"(_a3)
            :
            : "cc", "memory" //, "d1", "d2", "d3", "d4", "d5", "d6", "d7", "a4", "a5", "a6"
            // NOTE: Register clobbers here cause compiletime errors, so used push/pop instead.
        );
        return _d0;
    }

    void p61Music() {
        register volatile const void* _a3 ASM("a3") = player;
        register volatile const void* _a6 ASM("a6") = (void*)custom;
        __asm volatile (
            "movem.l %%d0-%%d7/%%a0-%%a2/%%a4-%%a5,-(%%sp)\n"
            "jsr 4(%%a3)\n"
            "movem.l (%%sp)+,%%d0-%%d7/%%a0-%%a2/%%a4-%%a5"
            : "+rf"(_a3), "+rf"(_a6)
            :
            : "cc", "memory"//, "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "a0", "a1", "a2", "a4", "a5"
            // NOTE: Register clobbers here cause compiletime errors, so used push/pop instead.
        );
    }

    void p61End() {
        register volatile const void* _a3 ASM("a3") = player;
        register volatile const void* _a6 ASM("a6") = (void*)custom;
        __asm volatile (
            //"movem.l %%d0-%%d1/%%a0-%%a1,-(%%sp)\n"
            "jsr 8(%%a3)\n"
            //"movem.l (%%sp)+,%%d0-%%d1/%%a0-%%a1"
            : "+rf"(_a3), "+rf"(_a6)
            :
            : "cc", "memory", "d0", "d1", "a0", "a1"
        );
    }
#endif //MUSIC

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
    return copWrite(copListEnd, (i<<8) | (x<<1) | 1, 0xfffe);
}

__attribute__((always_inline)) inline
USHORT* copWaitY(USHORT* copListEnd, USHORT i) {
    return copWrite(copListEnd, (i<<8) | 4 | 1, 0xfffe);
}

static __attribute__((interrupt)) void interruptHandler() {
    custom->intreq = INTF_VERTB;
    custom->intreq = INTF_VERTB; //reset vbl req. twice for a4000 bug.
#ifdef MUSIC
    // DEMO - ThePlayer
    p61Music();
#endif
    // DEMO - increment frameCounter
    frameCounter++;
}

#ifdef __cplusplus
    class TestClass {
    public:
        TestClass(int y) {
            static int x = 7;
            i = y + x;
        }
        ~TestClass() {
            KPrintF("~TestClass()");
        }

        int i;
    };

    TestClass staticClass(4);
#endif


// set up a 320x240 lowres display
__attribute__((always_inline)) inline USHORT* screenScanDefault(USHORT* copListEnd) {
    const USHORT width = SCREEN_WIDTH;
    const USHORT height = SCREEN_HEIGHT;
    const USHORT x = 129;
    const USHORT y = 44;
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

static void Wait10() { WaitLine(0x10); }
static void Wait11() { WaitLine(0x11); }
static void Wait12() { WaitLine(0x12); }
static void Wait13() { WaitLine(0x13); }

int main() {

    SysBase = *((struct ExecBase**)4UL);

    // We will use the graphics library only to locate and restore the system copper list once we are through.
    GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR)"graphics.library",0);
    if (!GfxBase)
        Exit(0);

    // used for printing
    DOSBase = (struct DosLibrary*)OpenLibrary((CONST_STRPTR)"dos.library", 0);
    if (!DOSBase)
        Exit(0);

#ifdef __cplusplus
    KPrintF("Hello debugger from Amiga: %ld!\n", staticClass.i);
#else
    KPrintF("Hello debugger from Amiga!\n");
#endif
    Write(Output(), (APTR)"\nDecahexagon debug build\n", 25);
    Delay(50);

    warpmode(1);
    // Precalc start

    // TODO: precalc stuff here
    init_tables();
    #ifdef MUSIC
    if(p61Init(module) != 0)
        KPrintF("p61Init failed!\n");
    #endif

    // Precalc end
    warpmode(0);

    TakeSystem();
    WaitVbl();

    // Allocate bitplanes
    bitplane_bg1 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg1 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_bg2 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg2 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    debug_register_bitmap(bitplane_bg1, "BG1", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg1, "FG1", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_bg2, "BG2", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg2, "FG2", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);

    USHORT* copper1 = (USHORT*)AllocMem(1024, MEMF_CHIP);
    USHORT* copPtr = copper1;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    //debug_register_bitmap(image, "image.bpl", 320, 256, 5, debug_resource_bitmap_interleaved);
    //debug_register_bitmap(bob, "bob.bpl", 32, 96, 5, debug_resource_bitmap_interleaved | debug_resource_bitmap_masked);
    //debug_register_palette(colors, "image.pal", 32, 0);
    debug_register_copperlist(copper1, "copper1", 1024, 0);
    debug_register_copperlist(copper2, "copper2", sizeof(copper2), 0);

    copPtr = screenScanDefault(copPtr);
    //enable bitplanes	
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon0), BPLCON0F_COLOR | (2*BPLCON0F_BPU210));
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon1), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon2), BPLCON2F_PF2PRI);

    //set bitplane modulo
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl1mod), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl2mod), 0);

    // set bitplane pointers
    void* copListSetBpl = copPtr;
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[0]), bitplane_bg1);
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg1);

    // set colors
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[0]), 0x000); // Even sector background / border
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[1]), 0x00f); // Odd sector background
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[2]), 0xf00); // Even sector wall
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[3]), 0xf0f); // Odd sector wall

    // jump to copper2
    *copPtr++ = offsetof(struct Custom, copjmp2);
    *copPtr++ = 0x7fff;

    custom->cop1lc = (ULONG)copper1;
    custom->cop2lc = (ULONG)copper2;
    custom->dmacon = DMAF_BLITTER;//disable blitter dma for copjmp bug
    custom->copjmp1 = 0x7fff; //start coppper
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    // DEMO
    SetInterruptHandler((APTR)interruptHandler);
    custom->intena = INTF_SETCLR | INTF_INTEN | INTF_VERTB;
#ifdef MUSIC
    custom->intena = INTF_SETCLR | INTF_EXTER; // ThePlayer needs INTF_EXTER
#endif

    custom->intreq=(1<<INTB_VERTB);//reset vbl req

    while(!MouseLeft()) {
        Wait10();
        int f = frameCounter & 255;

        // Debug: Put junk on bitplane to check cls
        ((UWORD*)bitplane_bg2)[0] = frameCounter;
        //bitplane_bg2[100] = frameCounter;

        // clear
        custom->color[0] = 0x700; // Red raster - Clearing bitplanes
        blit_cls(bitplane_bg2);
        blit_cls(bitplane_fg2);
        blit_wait();
        custom->color[0] = 0x222; // Black raster
        UWORD angle = gamestate.field_angle >> 6;
        gamestate.field_angle += gamestate.field_rotation;
        WORD x, y;
        radial_to_cartesian(angle, 50, &x, &y);
        blit_line(
            SCREEN_WIDTH/2, SCREEN_HEIGHT/2,
            SCREEN_WIDTH/2+50, SCREEN_HEIGHT/2,
            bitplane_bg2
        );
        blit_line(
            SCREEN_WIDTH/2, SCREEN_HEIGHT/2,
            SCREEN_WIDTH/2+x, SCREEN_HEIGHT/2+y,
            bitplane_bg2
        );

        // Flip render buffers on next frame
        copPtr = copWritePtr(copListSetBpl, offsetof(struct Custom, bplpt[0]), bitplane_bg2);
        copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg2);
        void* tmp = bitplane_bg1;
        bitplane_bg1 = bitplane_bg2;
        bitplane_bg2 = tmp;
        tmp = bitplane_fg1;
        bitplane_fg1 = bitplane_fg2;
        bitplane_fg2 = tmp;

        // WinUAE debug overlay test
        debug_clear();
        // debug_filled_rect(f + 100, 200*2, f + 400, 220*2, 0x0000ff00); // 0x00RRGGBB
        // debug_rect(f + 90, 190*2, f + 400, 220*2, 0x000000ff); // 0x00RRGGBB
        // debug_text(f+ 130, 209*2, "This is a WinUAE debug overlay", 0x00ff00ff);
    }

#ifdef MUSIC
    p61End();
#endif

    // END
    FreeSystem();

    CloseLibrary((struct Library*)DOSBase);
    CloseLibrary((struct Library*)GfxBase);
}

void init_tables() {
    register volatile const void* _a0 ASM("a0") = sin_table;
    __asm volatile (
        "    eor.w   %%d0,%%d0\n"
        "    moveq   #5,%%d1\n"
        "    swap    %%d1\n"
        "    move.w  #511+2,%%a1\n"
        "1:  move.l %%d0,%%d3\n"
        "    move.l  %%d1,%%d2\n"
        "    lsl.l   #8,%%d3\n"
        "    lsl.l   #8-3,%%d3\n"
        "    lsr.l   #3,%%d2\n"
        "    divu.w  %%d2,%%d3\n"
        "    move.w  %%d3,(%%a0)+\n"
        "    neg.w   %%d3\n"
        "    move.w  %%d3,(1022,%%a0)\n"
        "    subq.l  #2,%%a1\n"
        "    sub.l   %%a1,%%d1\n"
        "    add.l   %%a1,%%d0\n"
        "    bne.b   1b\n"
        : "+rf"(_a0)
        :
        : "cc", "memory", "d0", "d1", "d2", "d3", "a1"
    );
}

__attribute__((always_inline)) inline
void blit_wait() {
    custom->dmacon = DMAF_SETCLR | DMAF_BLITHOG;
    UWORD dummy = custom->dmaconr; // Dummy read for thin Agnus compatibility
    while (custom->dmaconr & DMAF_BLTDONE);
    custom->dmacon = DMAF_BLITHOG;
}

void blit_line(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    APTR startpt = bitplane + SCREEN_WIDTH_BYTES * y0 + ((x0 >> 4) << 1);
    WORD ed = x1 - x0; // Positive in east direction
    WORD sd = y1 - y0; // Positive in south direction
    WORD ne = ed - sd; // Positive in ne direction
    WORD se = ed + sd; // Positive in se direction

    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;

    if (se < 0) {
        // Octant 1234 Northwest
        if (ne < 0) {
            // Octant 34 West
            // x is major axis
            maj_d = -ed;
            if (sd < 0) {
                // Octant 3
                bltcon1 = SUD | SUL | AUL | LINEMODE;
                min_d = -sd;
            } else {
                // Octant 4
                bltcon1 = SUD | AUL | LINEMODE;
                min_d = sd;
            }
        } else {
            // Octant 12 North predominant
            // SUD = 0
            maj_d = -sd;
            if (ed < 0) {
                // Octant 2
                bltcon1 = SUL | AUL | LINEMODE;
                min_d = -ed;
            } else {
                // Octant 1
                bltcon1 = AUL | LINEMODE;
                min_d = ed;
            }
        }
    } else {
        // Octant 0567 Southeast
        // AUL = 0
        if (ne < 0) {
            // South predominant
            // Octant 5 6
            // SUD = 0
            maj_d = sd;
            if (ed < 0) {
                // Octant 5
                bltcon1 = SUL | LINEMODE;
                min_d = -ed;
            } else {
                // Octant 6
                bltcon1 = LINEMODE;
                min_d = ed;
            }
        } else {
            // East predominant
            // Octant 0 7
            maj_d = ed;
            if (sd < 0) {
                // Octant 0 SUL = 1
                bltcon1 = SUD | SUL | LINEMODE;
                min_d = -sd;
            } else {
                // Octant 7 SUL = 0
                bltcon1 = SUD | LINEMODE;
                min_d = sd;
            }
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d
    maj_d <<= 1;
    WORD bltbmod = min_d << 2; // 4min_d
    WORD bltaptl = bltbmod - maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d
    if (bltaptl < 0) {
        //bltaptl = -bltaptl;
        bltcon1 |= SIGNFLAG;
    }
    // Set starting word
    // Set starting bit
    UWORD bltcon0 = (x0 & 0xf) << 12;
    // Set DMA channels
    bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABC | ABNC | NABC | NANBC;  // or
    //bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC;  // xor

    // Spin until blitter free
    blit_wait();
    // Set up
    custom->bltadat = 0x8000;
    custom->bltbdat = 0xffff;
    custom->bltafwm = 0xffff;
    custom->bltalwm = 0xffff;
    custom->bltamod = bltamod;
    custom->bltbmod = bltbmod;
    custom->bltcmod = SCREEN_WIDTH_BYTES;
    custom->bltdmod = SCREEN_WIDTH_BYTES;
    custom->bltapt = (APTR)((ULONG)bltaptl);
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = bltcon0;
    custom->bltcon1 = bltcon1;
    custom->bltsize = ((maj_d + 1) << 6) | 2;
}

__attribute__((always_inline)) inline
void blit_cls(void *bitplane) {
    register volatile const void* _a0 ASM("a0") = bitplane;
    register volatile const struct Custom* _a6 ASM("a6") = custom;
    __asm volatile (
        "        move.w  #0xc000,0x96(%%a6)\n" // Turn on BLITHOG
        "        btst.b  #6,2(%%a6)\n" // 14-8, dmaconr Thin Agnus compability
        "1:      btst.b  #6,2(%%a6)\n" // 14-8, dmaconr
        "        bne.b   1b\n"
        "        move.w  #0x4000,0x96(%%a6)\n" // Turn off BLITHOG
        "        move.w  #0x0100,0x40(%%a6)\n" // bltcon0
        "        move.w  #0x0000,0x42(%%a6)\n" // bltcon1
        "        move.w  #0xffff,0x44(%%a6)\n" // bltafwm
        "        move.w  #0xffff,0x46(%%a6)\n" // bltalwm
        "        move.l  %%a0,0x54(%%a6)\n" // bltdpt
        "        move.w  #0x0000,0x66(%%a6)\n" // bltdmod
        "        move.w  #0x3c14,0x58(%%a6)\n" // bltsize 320x240
        :
        : "r"(_a0), "r"(_a6)
        : "cc", "memory"
    );
}

__attribute__((always_inline)) inline
void blit_fill(void *bitplane) {
    // Blit fill works backwards. Point to last word in screen buffer.
    register volatile const void* _a0 ASM("a0") = bitplane + (40*240-2);
    register volatile const struct Custom* _a6 ASM("a6") = custom;
    // Must use DESC mode
    // Use FILL_XOR mode
    // BLTCON0 0000_1001_1111_0000
    // BLTCON1 0000_0000_0000_1010
    __asm volatile (
        "        move.w  #0xc000,0x96(%%a6)\n" // Turn on BLITHOG
        "        btst.b  #6,2(%%a6)\n" // 14-8, dmaconr Thin Agnus compability
        "1:      btst.b  #6,2(%%a6)\n" // 14-8, dmaconr
        "        bne.b   1b\n"
        "        move.w  #0x4000,0x96(%%a6)\n" // Turn off BLITHOG
        "        move.l  #0x000a09f0,0x40(%%a6)\n" // bltcon0/1
        "        move.l  #0xffffffff,0x44(%%a6)\n" // bltafwm/bltalwm
        "        move.l  %%a0,0x50(%%a6)\n" // bltapt
        "        move.l  %%a0,0x54(%%a6)\n" // bltdpt
        "        move.l  #0x00000000,0x64(%%a6)\n" // bltamod/bltdmod
        "        move.w  #0x3c14,0x58(%%a6)\n" // bltsize 320x240
        :
        : "r"(_a0), "r"(_a6)
        : "cc", "memory"
    );
}

__attribute__((always_inline)) inline
void blit_fill_even(void *bitplane) {
    // Blit fill works backwards. Point to last word in screen buffer.
    register volatile const void* _a0 ASM("a0") = bitplane + (40*240-40-2);
    register volatile const struct Custom* _a6 ASM("a6") = custom;
    // Must use DESC mode
    // Use FILL_XOR mode
    // BLTCON0 0000_1001_1111_0000
    // BLTCON1 0000_0000_0000_1010
    __asm volatile (
        "        move.w  #0xc000,0x96(%%a6)\n" // Turn on BLITHOG
        "        btst.b  #6,2(%%a6)\n" // 14-8, dmaconr Thin Agnus compability
        "1:      btst.b  #6,2(%%a6)\n" // 14-8, dmaconr
        "        bne.b   1b\n"
        "        move.w  #0x4000,0x96(%%a6)\n" // Turn off BLITHOG
        "        move.l  #0xa09f0,0x40(%%a6)\n" // bltcon0/1
        "        move.l  #0xffffffff,0x44(%%a6)\n" // bltafwm/bltalwm
        "        move.l  %%a0,0x50(%%a6)\n" // bltapt
        "        move.l  %%a0,0x54(%%a6)\n" // bltdpt
        "        move.l  #0x280028,0x64(%%a6)\n" // bltamod/bltdmod = 40 (320 bits = 1 line)
        "        move.w  #0x1e14,0x58(%%a6)\n" // bltsize 320x120
        :
        : "r"(_a0), "r"(_a6)
        : "cc", "memory"
    );
}

void radial_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y) {
    *y = (sin_table[angle & 0x3ff] * length) >> 14;
    *x = (sin_table[(angle + 0x100) & 0x3ff] * length) >> 14;
}

void background_outlines() {
    // Draw solid around segments 1, 3, 5    
}

void render(void * bitplane_bg, void * bitplane_fg) {
    // Update rotations
    gamestate.field_angle += gamestate.field_rotation;

    // Frame takes 160k cycles.
    BLITHOG_ON;
    blit_cls(bitplane_bg); // 4.8k DMAs
    blit_cls(bitplane_fg); // 4.8k DMAs
    BLITHOG_OFF;

    // Draw background

    BLITHOG_ON;
    blit_fill(bitplane_bg); // 9.6k DMAs
    blit_fill(bitplane_fg); // 9.6k DMAs
    BLITHOG_OFF;
    // Set player sprite position.
    // Setup score sprites.
}

// Top left display: BEST: XXX:XX also shows level progress
// Top right display: TIME XXX:XX


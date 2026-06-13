#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include "custom2.h"
#include <proto/dos.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>
#include <graphics/gfxmacros.h>
//#include <hardware/custom.h>
#include <hardware/dmabits.h>
#include <hardware/intbits.h>
#include <hardware/adkbits.h>
#include <hardware/cia.h>
#include "paulabits.h"
#include "include/pt.h"

//config
//#define MUSIC
#define MUSIC_LSP
#define ASM_OPT
//#define SHOW_DRAW_PLANE
//#define SKIP_FILL
//#define DEBUG_NAG
#define DEBUG_SPAG

#define MAX_NUM_SIDES (12)
#define NUM_SIDES MAX_NUM_SIDES

#define SCREEN_WIDTH (320) // Currently fixed at 320 due to cls routine
#define SCREEN_HEIGHT (200) // Must be multiple of 4 for cls routine
#define FRAME_RATE (50) // To keep difficulty consistent between PAL and NTSC
#define SCREEN_WIDTH_BYTES (SCREEN_WIDTH >> 3)

// Pixel aspect correction for NTSC
#if FRAME_RATE == 60
// Should be *0.8333
// n - (n>>2) = 0.75
// n - (n>>2) + (n>>4) = 0.8125 close enough

#define PIXEL_ASPECT_CORRECT_Y(n) (n * 5 / 6)
#else
// Strictly this is 15/16, but 1 is close enough
#define PIXEL_ASPECT_CORRECT_Y(n) (n)
#endif

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

UWORD *bitplane_fg1;
UWORD *bitplane_fg2;
UWORD *bitplane_fg3;

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
    .field_rotation = 65536 / FRAME_RATE * 3 / 6, // 1 degree per 60Hz frame
    .segment_angle = ((65536 + NUM_SIDES - 1) / NUM_SIDES), // Ensure overflow after last segment
    .segment_angle_target = ((65536 + NUM_SIDES - 1) / NUM_SIDES),
    .player_angle = 0,
    .wall_fraction = 0,
    .draw_distance = 0x500,
    .draw_distance_target = 0x500,
    .time_seconds = 16,
    .time_subsecond_frames = 10,
    .record_seconds = 126,
    .record_subsecond_frames = 10
};

// Graphics routines targetting blitter
__attribute__((always_inline)) inline void blit_cls(void *bitplane);
__attribute__((always_inline)) inline void cpu_cls(void *bitplane);
__attribute__((always_inline)) inline void blit_line_onedot(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);
__attribute__((always_inline)) inline void blit_line(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);
__attribute__((always_inline)) inline void blit_wait();
__attribute__((always_inline)) inline void blit_fill(void *bitplane, void* bitplane2);
void blit_fill_fix_onedot(WORD y0, WORD y1, void *bitplane);
void blit_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane
);
void blit_line_mode();

// Graphics routines targetting copperlist
__attribute__((always_inline)) inline WORD* cl_cls(WORD* bitplane, WORD* copPtr);
__attribute__((always_inline)) inline WORD* cl_line_mode(WORD* copPtr);
__attribute__((always_inline)) inline WORD* cl_line_onedot(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void * bitplane, WORD* copPtr);
__attribute__((always_inline)) inline WORD* cl_fill(void *bitplane, void *bitplane2, WORD* copPtr);
__attribute__((always_inline)) inline WORD* cl_fill_fix_onedot(WORD y0, WORD y1, void *bitplane, WORD* copPtr);
__attribute__((always_inline)) inline WORD* cl_clipped_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane, WORD* copPtr);

WORD sin_table[1024];

void init_tables();
__attribute__((always_inline)) inline USHORT* copWrite(USHORT* copListEnd, UWORD offset, UWORD data);
void polar_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y);

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

APTR GetInterruptHandler() {
    return *(volatile APTR*)(((UBYTE*)VBR) + 0x6c);
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

__attribute__((always_inline)) inline void WaitBlt() {
    *(volatile UWORD*)&custom->dmaconr;
    while (*(volatile UWORD*)&custom->dmaconr & DMAF_BLTDONE);
}

void TakeSystem() {
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

void FreeSystem() { 
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

__attribute__((always_inline)) inline short MouseLeft() {
    return !(ciaa->ciapra & CIAB_GAMEPORT0);
}	


__attribute__((always_inline)) inline short MouseRight(){
    return !(custom->potinp & POTINF_L_MOUSE_BUT2);
}

volatile short frameCounter = 0;

// Put copperlist into chip mem so we can use it without copying
const UWORD copper2[] __attribute__((section (".MEMF_CHIP"))) = {
    0xffff, 0xfffe // End copper list
};

void* doynaxdepack(const void* input, void* output) { // returns end of output data, input needs to be 16-bit aligned!
    register volatile const void* _a0 ASM("a0") = input;
    register volatile       void* _a1 ASM("a1") = output;
    __asm volatile (
        "movem.l %%d0-%%d7/%%a2-%%a6,-(%%sp)\n"
        "jsr     _doynaxdepack_vasm\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a2-%%a6"
    : "+rf"(_a0), "+rf"(_a1)
    :
    : "cc", "memory");
    return (void*)_a1;
}

#ifdef MUSIC_LSP
INCBIN_CHIP(LSPMusic, "technova_main.lsmusic");
INCBIN_CHIP(LSPBank, "technova_main.lsbank");

int p61Init() {
    register volatile void* _a0 ASM("a0") = (void*) LSPMusic;
    register volatile void* _a1 ASM("a1") = (void*) LSPBank;
    register volatile void* _a2 ASM("a2") = 0; // VBR
    register volatile void* _d0 ASM("d0") = 0; // PAL
    __asm volatile (
        "movem.l %%d1/%%a3/%%a5,-(%%sp)\n"
        "jsr LSP_MusicDriver_CIA_Start\n"
        "movem.l (%%sp)+,%%d1/%%a3/%%a5\n"
        : "+rf"(_a0), "+rf"(_a1), "+rf"(_a2), "+rf"(_d0)
        : 
        : "cc", "memory"
    );
    return 0;
}

int p61Music() {
    // Not needed for LSP CIA
}

int p61End() {
}
#endif
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
    return copWrite(copListEnd, (i << 8) | (x << 1) | 1, 0xfffe);
}

__attribute__((always_inline)) inline
USHORT* copWaitY(USHORT* copListEnd, USHORT i) {
    return copWrite(copListEnd, (i << 8) | 4 | 1, 0xfffe);
}

static __attribute__((interrupt)) void interruptHandler() {
    custom->intreq = INTF_VERTB;
    custom->intreq = INTF_VERTB; // Reset vbl req. twice for a4000 bug.
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
    const USHORT x = 129 + (SCREEN_WIDTH - 320) / 2;
    const USHORT y = 44 + (SCREEN_WIDTH - 256) / 2;
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

WORD frame_sin[MAX_NUM_SIDES];
WORD frame_cos[MAX_NUM_SIDES];

int main() {

    SysBase = *((struct ExecBase**)4UL);

    // We will use the graphics library only to locate and restore the system copper list once we are through.
    GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR)"graphics.library",0);
    if (!GfxBase) Exit(0);

    // Used for printing
    DOSBase = (struct DosLibrary*)OpenLibrary((CONST_STRPTR)"dos.library", 0);
    if (!DOSBase) Exit(0);

#ifdef __cplusplus
    KPrintF("Hello debugger from Amiga: %ld!\n", staticClass.i);
#else
    KPrintF("Hello debugger from Amiga!\n");
#endif

#ifdef DEBUG_NAG
    Write(Output(), (APTR)"\nDecahexagon debug build for Norwich Amiga Group\n", 50);
#elif defined(DEBUG_NGD)
    Write(Output(), (APTR)"\nDecahexagon debug build for Norfolk Game Developers\n", 54);
#elif defined(DEBUG_SPAG)
    Write(Output(), (APTR)"\nDecahexagon debug build for SPAG\n", 34);
#endif

    Delay(50);

    warpmode(1);
    // Precalc start

    // Generate sin table
    init_tables();

    #ifdef MUSIC
    if(p61Init(module) != 0)
        KPrintF("p61Init failed!\n");
    #endif
    #ifdef MUSIC_LSP
    p61Init();
    #endif

    // Precalc end
    warpmode(0);

    TakeSystem();
    WaitVbl();

    // Allocate bitplanes
    bitplane_fg1 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg2 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg3 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    
    USHORT* copper1 = (USHORT*)AllocMem(1024, MEMF_CHIP);
    USHORT* copPtr = copper1;
    
    // Register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_bitmap(bitplane_fg1, "FG1", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg2, "FG2", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg3, "FG3", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_copperlist(copper1, "copper1", 1024, 0);
    debug_register_copperlist(copper2, "copper2", sizeof(copper2), 0);

    copPtr = screenScanDefault(copPtr);
    // Enable bitplanes	
    #ifdef SHOW_DRAW_PLANE
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon0), BPLCON0F_COLOR | (2 * BPLCON0F_BPU210));
    #else
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon0), BPLCON0F_COLOR | (1 * BPLCON0F_BPU210));
    #endif
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon1), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon2), BPLCON2F_PF2PRI);

    // Set bitplane modulo
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl1mod), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl2mod), 0);

    // Set bitplane pointers
    void* copListSetBpl = copPtr;
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[0]), bitplane_fg1);
    #ifdef SHOW_DRAW_PLANE
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg2);
    #endif

    // Jump to copper2
    *copPtr++ = offsetof(struct Custom, copjmp2);
    *copPtr++ = 0x7fff;

    custom->cop1lc = (ULONG)copper1;
    custom->cop2lc = (ULONG)copper2;
    custom->dmacon = DMAF_BLITTER; // Disable blitter dma for copjmp bug
    custom->copjmp1 = 0x7fff; // Start copper
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER | DMAF_BLITHOG;

    // DEMO
    SetInterruptHandler((APTR)interruptHandler);
    custom->intena = INTF_SETCLR | INTF_INTEN | INTF_VERTB;
#ifdef MUSIC
    custom->intena = INTF_SETCLR | INTF_EXTER; // ThePlayer needs INTF_EXTER
#endif
#ifdef MUSIC_LSP
    custom->intena = INTF_SETCLR | INTF_EXTER; // LightSpeed Player CIA mode needs INTF_EXTER
#endif

    custom->intreq = (1 << INTB_VERTB); // Reset vbl req

    while(!MouseLeft()) {
        Wait10();
        int f = frameCounter & 255;

        UWORD field_angle = gamestate.field_angle;
        gamestate.field_angle += gamestate.field_rotation;
        WORD x, y, new_x, new_y;

        UWORD scale = (SCREEN_HEIGHT / 4) + ((frameCounter >> 2) & 0x1f);
        // Calculate unit vectors

        // Build frame specific sin/cos table
        UWORD angle = field_angle;
        for (WORD i = 0; i < MAX_NUM_SIDES; i++) {
            UWORD ang_shift = angle >> 6;
            frame_sin[i] = sin_table[ang_shift];
            WORD tmp = sin_table[(ang_shift + 0x100) & 0x3ff];
            frame_cos[i] = tmp - (tmp >> 2);
            angle += gamestate.segment_angle;
        }

        blit_line_mode();
        for (WORD i = 6; i>0; i--) {
            UWORD draw_angle = 0;

            x = frame_sin[0];
            asm(
                "mulsw %[scale],%[x]\n"
                "lsl.l #2,%[x]\n"
                "swap %[x]\n"
                : [x]"+&d"(x)
                : [scale]"d"(scale)
                : "cc"
            );
            WORD end_x = x;
            y = frame_cos[0];
            asm(
                "mulsw %[scale],%[y]\n"
                "lsl.l #2,%[y]\n"
                "swap %[y]\n"
                : [y]"+&d"(y)
                : [scale]"d"(scale)
                : "cc"
            );
            WORD end_y = y;
            for (WORD j = 1; j<NUM_SIDES; j++) {
                new_x = frame_sin[j];
                asm(
                    "mulsw %[scale],%[new_x]\n"
                    "lsl.l #2,%[new_x]\n"
                    "swap %[new_x]\n"
                    : [new_x]"+&d"(new_x)
                    : [scale]"d"(scale)
                    : "cc"
                );
                new_y = frame_cos[j];
                asm(
                    "mulsw %[scale],%[new_y]\n"
                    "lsl.l #2,%[new_y]\n"
                    "swap %[new_y]\n"
                    : [new_y]"+&d"(new_y)
                    : [scale]"d"(scale)
                    : "cc"
                );
                blit_clipped_line_onedot(
                    SCREEN_WIDTH / 2 + x, SCREEN_HEIGHT / 2 + y,
                    SCREEN_WIDTH / 2 + new_x, SCREEN_HEIGHT / 2 + new_y,
                    0,
                    bitplane_fg2
                );
                x = new_x;
                y = new_y;
            }
            // Draw last line back to start point
            blit_clipped_line_onedot(
                SCREEN_WIDTH / 2 + x, SCREEN_HEIGHT / 2 + y,
                SCREEN_WIDTH / 2 + end_x, SCREEN_HEIGHT / 2 + end_y,
                0,
                bitplane_fg2
            );
            scale += 25;
        }

        #ifndef SKIP_FILL
        blit_fill(bitplane_fg2, bitplane_fg2);
        #endif
        cpu_cls(bitplane_fg3);

        // Flip render buffers on next frame
        copPtr = copWritePtr(copListSetBpl, offsetof(struct Custom, bplpt[0]), bitplane_fg2);
        #ifdef SHOW_DRAW_PLANE
        copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg3);
        #endif

        UWORD new_palette_angle = frameCounter & 0x3ff;
        UWORD new_palette_red = 8 + ((sin_table[new_palette_angle] * 7) >> 14);
        UWORD new_palette_green = 8 + ((sin_table[(new_palette_angle + 1024 / 3) & 0x3ff] * 7) >> 14);
        UWORD new_palette_blue = 8 + ((sin_table[(new_palette_angle + 2 * 1024 / 3) & 0x3ff] * 7) >> 14);
        UWORD palette = ((new_palette_red & 0xf) << 8) + ((new_palette_green & 0xf) << 4) + (new_palette_blue & 0xf);
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[0]), (palette >> 1) & 0x777);
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[1]), palette);
        
        // Bitplane fg3: Blank bitplane
        // Bitplane fg2: Line rendering and fill
        // Bitplane fg1: Display

        void* tmp = bitplane_fg1;
        bitplane_fg1 = bitplane_fg2;
        bitplane_fg2 = bitplane_fg3;
        bitplane_fg3 = tmp;

        // WinUAE debug overlay test
        // debug_clear();
        // debug_filled_rect(f + 100, 200*2, f + 400, 220*2, 0x0000ff00); // 0x00RRGGBB
        // debug_rect(f + 90, 190*2, f + 400, 220*2, 0x000000ff); // 0x00RRGGBB
        // debug_text(f+ 130, 209*2, "This is a WinUAE debug overlay", 0x00ff00ff);

        custom->color[0] = 0x800; // Black raster - all done
        blit_wait();
    }

#ifdef MUSIC
    p61End();
#endif

    // END
    FreeSystem();

    CloseLibrary((struct Library*)DOSBase);
    CloseLibrary((struct Library*)GfxBase);
}

__attribute__((always_inline)) inline
void init_tables() {
    register volatile const void* _a0 ASM("a0") = sin_table;
    __asm volatile (
        "    moveq   #0,%%d0\n"
        "    moveq   #5,%%d1\n"
        "    swap    %%d1\n"
        "    move.w  #511+2,%%a1\n"
        "1:  move.l  %%d0,%%d3\n"
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
    UWORD dummy = custom->dmaconr; // Dummy read for thin Agnus compatibility
    while (custom->dmaconr & DMAF_BLTDONE);
}

void blit_line_mode() {
    blit_wait();
    // Preload registers for line mode activities
    // Saves resetting them every line.
    custom->bltbdat = 0xffff;
    custom->bltafwm_lwm = 0xffffffff;
    custom->bltcmod_bmod = (SCREEN_WIDTH_BYTES << 16) | SCREEN_WIDTH_BYTES;
}

#define XMAX (SCREEN_WIDTH-1)
#define YMAX (SCREEN_HEIGHT-1)
#define FRACBITS 8

void blit_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane
) {
    // Draws from x0/y0 to x1/y1. Inclusive of lowest y, exclusive of largest y.
    WORD outside_viewport = 4;
    WORD viewport_intersection = 0;
    WORD mxy = 0;
    WORD myx = 0;

    // Clip at y=0
    if (y0 > y1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (y1 < 0) {
        // Entire line is above screen. Discard.
        return;
    } else if (y0 < 0) {
        // Test for intersection with top of screen
        #ifdef ASM_OPT
        mxy = x1 - x0;
        WORD yd = y1 - y0;
        asm(
            "ext.l  %[mxy]\n"
            "asl.l  %[fracbits],%[mxy]\n"
            "divs.w %[yd],%[mxy]\n"
            : [mxy]"+&d"(mxy)
            : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y0]"d"(y0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x0 - result;
        #else
        mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
        WORD new_x = x0 - ((y0 * mxy) >> FRACBITS);
        #endif
        
        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects top of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = new_x;
            y0 = 0;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at y=YMAX
    if (y0 > YMAX) {
        // Entire line is below screen. Discard.
        return;
    } else if (y1 > YMAX) {
        if (!mxy) {
            #ifdef ASM_OPT
            mxy = x1 - x0;
            WORD yd = y1 - y0;
            asm(
                "ext.l  %[mxy]\n"
                "asl.l  %[fracbits],%[mxy]\n"
                "divs.w %[yd],%[mxy]\n"
                : [mxy]"+&d"(mxy)
                : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
            #else
            mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
            #endif
        }

        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y1]"d"(YMAX - y1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x1 + result;
        #else
        WORD new_x = x1 + (((YMAX - y1) * mxy) >> FRACBITS);
        #endif

        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects bottom of screen. Move x1/y1 point onscreen and flag the intersection.
            x1 = new_x;
            y1 = YMAX;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at x=0
    if (x0 > x1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (x1 < 0) {
        // Entire line is left of screen. Discard.
        return;
    } else if (x0 < 0) {
        #ifdef ASM_OPT
        myx = y1 - y0;
        WORD xd = x1 - x0;
        asm(
            "ext.l  %[myx]\n"
            "asl.l  %[fracbits],%[myx]\n"
            "divs.w %[xd],%[myx]\n"
            : [myx]"+&d"(myx)
            : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x0]"d"(x0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y0 - result;
        #else
        myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
        WORD new_y = y0 - ((x0 * myx) >> FRACBITS);
        #endif
        
        if (new_y >= 0 && new_y <= YMAX) {
            // Line intersects left of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = 0;
            y0 = new_y;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }
    if (x0 > XMAX) {
        // Entire line is right of screen. But still need to get fill state correct.
        blit_fill_fix_onedot(y0, y1, bitplane);
        return;
    } else if (x1 > XMAX) {
        if (!myx) {
            #ifdef ASM_OPT
            myx = y1 - y0;
            WORD xd = x1 - x0;
            asm(
                "ext.l  %[myx]\n"
                "asl.l  %[fracbits],%[myx]\n"
                "divs.w %[xd],%[myx]\n"
                : [myx]"+&d"(myx)
                : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
            #else
            myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
            #endif
        }
        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x1]"d"(XMAX - x1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y1 + result;
        #else
        WORD new_y = y1 + (((XMAX - x1) * myx) >> FRACBITS);
        #endif
        
        if (new_y < 0) {
            // TODO: Is this needed?
            blit_fill_fix_onedot(0, y1, bitplane);
        } else if (new_y > YMAX) {
            // TODO: Is this needed?
            blit_fill_fix_onedot(y1, YMAX, bitplane);
        } else {
            blit_fill_fix_onedot(y1, new_y, bitplane);
            x1 = XMAX;
            y1 = new_y;
            viewport_intersection = 1;
        }
        // TODO: What kind of lines are in the else clause here? Do they also need a fillfix?
        // They cross the x=XMAX line, but not on-screen.
    } else {
        outside_viewport -= 1;
    }
    if (outside_viewport == 0 || viewport_intersection) {
        blit_line_onedot(x0, y0, x1, y1, bitplane);
    }
}

void blit_line_onedot(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // Draws a line from x0,y0 to x1,y1.
    // Pixels x0,y0 and x1,y1 are guaranteed to be drawn.
    // 
    // See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
    //

    // Horizontal lines already have a pixel at start and end from other edges.
    // No drawing required.
    if (y0 == y1) return;

    // Swap end points to draw in a south/easterly direction (Octants 4 5 6 7 only)
    if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }

    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    // Note octants 0, 1, 2, 3 are omitted as they are never drawn.

    APTR startpt = bitplane + muluw(y0, SCREEN_WIDTH_BYTES) + ((x0 >> 4) << 1);
    WORD ed = x1 - x0; // Positive in east direction
    UWORD sd = y1 - y0; // Positive in south direction, guaranteed to be positive
    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;
    if (ed + sd < 0) {
        // Octant 4
        maj_d = -ed;
        min_d = sd;
        bltcon1 = SUD | AUL | ONEDOT | LINEMODE;
    } else {
        // Octant 0567 Southeast
        if (ed - sd < 0) {
            // South predominant
            maj_d = sd;
            if (ed < 0) {
                // Octant 5
                min_d = -ed;
                bltcon1 = SUL | LINEMODE; // ONEDOT doesn't actually do anything for this octant
            } else {
                // Octant 6
                min_d = ed;
                bltcon1 = LINEMODE; // ONEDOT doesn't actually do anything for this octant
            }
        } else {
            // East predominant
            // Octant 7
            maj_d = ed;
            min_d = sd;
            bltcon1 = SUD | ONEDOT | LINEMODE;
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d

    WORD bltbmod = min_d << 2; // 4min_d
    maj_d <<= 2;
    WORD bltamod = bltbmod - maj_d; // 4 min_d - 4 maj_d
    WORD bltaptl = bltamod; // This goes against HRM, but seems to work well.
    if (bltaptl < 0) bltcon1 |= SIGNFLAG;

    // Set starting word, DMA channels and logic function
    // TODO: Does this skip the first pixel? BC0F_DEST is not set.
    // See https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // https://eab.abime.net/showpost.php?p=206412&postcount=6
    //
    // FIXME: This appears to draw the first pixel, but does not draw the last pixel. 
    UWORD bltcon0 = (
        (x0 & 0xf) << 12 // Starting bit within word
        | BC0F_SRCC | BC0F_SRCA // Missing DEST here
        | ABNC | NABC | NANBC // 4a xor
    );
    // Spin until blitter free
    blit_wait();

    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = bltbmod;
    custom->bltapt_l = bltaptl;
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = bltcon0;
    custom->bltcon1 = bltcon1;
    custom->bltsize = (maj_d << 4) + 2;
}

void blit_fill_fix_onedot(
    WORD y0, WORD y1, void *bitplane
) {
    // Draws from ymin (inclusive) to ymax (exclusive)
    if (y0 > y1) {
        WORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    // Skip offscreen
    if (y1 < 0) return;
    if (y0 > YMAX) return;
    // Clip to screen
    if (y0 < 0) y0 = 0;
    if (y1 > YMAX) y1 = YMAX;
    // Skip zero length lines
    if (y1 == y0) return;

    APTR startpt = (
        bitplane
        + muluw(y0, SCREEN_WIDTH_BYTES)
        + (SCREEN_WIDTH >> 3) - 2
    ); // Location of rightmost word
    UWORD maj_d = (y1 - y0) << 1;
    WORD bltaptl = -maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d
    // Spin until blitter free
    blit_wait();
    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = 0;
    custom->bltapt = (APTR)((ULONG)bltaptl);
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = (
        (0xf << 12)             // Rightmost word
        | BC0F_SRCC | BC0F_SRCA // Set DMA channels
        | ABNC | NABC | NANBC   // 4a xor
    );
    custom->bltcon1 = LINEMODE | SIGNFLAG;
    custom->bltsize = (maj_d << 5) + 2; // Remember maj_d was doubled above
}

void blit_line(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // TODO: First pixel? Last pixel?

    // Draws a line from x0,y0 to x1,y1.
    // Pixels x0,y0 and x1,y1 are guaranteed to be drawn.
    // 
    // See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
    //
    if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }

    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    APTR startpt = (
        bitplane
        + muluw(y0, SCREEN_WIDTH_BYTES)
        + ((x0 >> 4) << 1)
    );
    WORD ed = x1 - x0; // Positive in east direction
    UWORD sd = y1 - y0; // Positive in south direction
    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;

    if (ed + sd < 0) {
        // x is major axis
        maj_d = -ed;
        // Octant 4
        min_d = sd;
        bltcon1 = SUD | AUL | LINEMODE;
    } else {
        // Octant 0567 Southeast
        // AUL = 0
        if (ed - sd < 0) {
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
            maj_d = ed;
            // Octant 7
            bltcon1 = SUD | LINEMODE;
            min_d = sd;
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d
    maj_d <<= 1;
    WORD bltbmod = min_d << 2; // 4min_d
    WORD bltaptl = bltbmod - maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d
    if (bltaptl < 0) bltcon1 |= SIGNFLAG;

    // Set starting word
    // Set starting bit
    UWORD bltcon0 = (x0 & 0xf) << 12;
    // Set DMA channels
    bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABC | ABNC | NABC | NANBC;  // or
    //bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC;  // xor
    //bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC;  // xor
    // Spin until blitter free
    blit_wait();
    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = bltbmod;
    custom->bltapt = (APTR)((ULONG)bltaptl);
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = bltcon0;
    custom->bltcon1 = bltcon1;
    custom->bltsize = (maj_d << 5) + ((1 << 6) + 2); // Remember maj_d was doubled above
}

__attribute__((always_inline)) inline
void blit_cls(void *bitplane) {
    blit_wait();
    custom->bltcon0 = BC0F_DEST;
    custom->bltcon1 = 0;
    //custom->bltafwm = 0xffff; Not required because propogated from line mode
    //custom->bltalwm = 0xffff; Not required because propogated from line mode
    custom->bltdpt = bitplane;
    custom->bltdmod = 0;
    custom->bltsize = (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1);
}

__attribute__((always_inline)) inline
void cpu_cls(void *bitplane) {
    register volatile const UWORD _d0 ASM("d0") = ((SCREEN_HEIGHT * SCREEN_WIDTH_BYTES) / 160) - 1;
    // This assumes 320 pixels wide and HEIGHT is a multiple of 4 lines
    register volatile const void* _a0 ASM("a0") = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES;
    __asm volatile (
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%sp)\n"
        "   moveq.l #0,%%d1\n"
        "   moveq.l #0,%%d2\n"
        "   moveq.l #0,%%d3\n"
        "   moveq.l #0,%%d4\n"
        "   moveq.l #0,%%d5\n"
        "   moveq.l #0,%%d6\n"
        "   moveq.l #0,%%d7\n"
        "   movea.l %%d1,%%a1\n"
        "   movea.l %%d1,%%a2\n"
        "   movea.l %%d1,%%a3\n"
        "1: movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   dbra    %%d0,1b\n"
        "   movem.l (%%sp)+,%%d1-%%d7/%%a1-%%a3\n"
        :
        : "rf"(_d0), "rf"(_a0)
        : "cc", "memory"
    );
}

__attribute__((always_inline)) inline
void blit_fill(void *bitplane, void *bitplane2) {
    APTR start = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    APTR start2 = bitplane2 + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    blit_wait();
    custom->bltcon0 = BC0F_SRCA | BC0F_DEST | A_TO_D;
    custom->bltcon1 = FILL_XOR | BLITREVERSE;
    custom->bltafwm = 0xffff;
    custom->bltalwm = 0xffff;
    custom->bltapt = start;
    custom->bltdpt = start2;
    custom->bltamod = 0;
    custom->bltdmod = 0;
    custom->bltsize = (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1);
}

__attribute__((always_inline)) inline
void polar_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y) {
    angle >>= 6; // Sin table has 1023 entries
    WORD result = sin_table[angle];
    //result = (result * length) >> 14;
    asm(
        "muls.w %[length],%[result]\n"
        "lsl.l #2,%[result]\n"
        "swap %[result]\n"
        : [result]"+&d"(result)
        : [length]"d"(length)
        : "cc"
    );
    result -= result >> 2;
    *y = result;

    result = sin_table[(angle + 0x100) & 0x3ff];
    //result = (result * length) >> 14;
    asm(
        "muls.w %[length],%[result]\n"
        "lsl.l #2,%[result]\n"
        "swap %[result]\n"
        : [result]"+&d"(result)
        : [length]"d"(length)
        : "cc"
    );
    *x = result;
}

// TODO: Add player

__attribute__((always_inline)) inline 
WORD* cl_cls(WORD* bitplane, WORD* copPtr) {
    // Wait blitter free
    copPtr = copWrite(copPtr, 0x0001, 0x0000);
    // Blit memory fill from static value (0)
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltcon0), BC0F_DEST);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltcon1), 0);
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bltdpt), bitplane);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltdmod), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltsize), (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1));
    return copPtr;
}

__attribute__((always_inline)) inline 
WORD* cl_line_mode(WORD* copPtr) {
    WORD copperdata[] = {
        0x0001, 0x0000,
        offsetof(struct Custom, bltbdat), 0xffff,
        offsetof(struct Custom, bltafwm), 0xffff,
        offsetof(struct Custom, bltalwm), 0xffff,
        offsetof(struct Custom, bltbmod), SCREEN_WIDTH_BYTES,
        offsetof(struct Custom, bltdmod), SCREEN_WIDTH_BYTES
    };
    memcpy(copPtr, copperdata, sizeof(copperdata));
    return copPtr + sizeof(copperdata);
}

// Draw line with onedot. Requires cl_line_mode already called.
__attribute__((always_inline)) inline 
WORD* cl_line_onedot(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void * bitplane,
    WORD* copPtr
) {
    // Draws a line from x0,y0 to x1,y1.
    // Pixels x0,y0 and x1,y1 are guaranteed to be drawn.
    // 
    // See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
    //

    // Horizontal lines already have a pixel at start and end from other edges.
    // No drawing required.
    if (y0 == y1) return copPtr;

    // Wait blitter free
    *copPtr++ = 0x0001;
    *copPtr++ = 0x0000;

    // Initalise BLTADAT
    *copPtr++ = offsetof(struct Custom, bltadat);
    *copPtr++ = 0x8000;

    // Swap end points to draw in a south/easterly direction (Octants 4 5 6 7 only)
    if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }

    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    // Note octants 0, 1, 2, 3 are omitted as they are never drawn.

    APTR startpt = bitplane + muluw(y0, SCREEN_WIDTH_BYTES) + ((x0 >> 4) << 1);
    *copPtr++ = offsetof(struct Custom, bltcpt);
    *copPtr++ = (int)startpt >> 16;
    *copPtr++ = offsetof(struct Custom, bltdpt);
    *copPtr++ = (int)startpt >> 16;
    *copPtr++ = offsetof(struct Custom, bltcpt) + 2;
    *copPtr++ = (int)startpt & 0xffff;
    *copPtr++ = offsetof(struct Custom, bltdpt) + 2;
    *copPtr++ = (int)startpt & 0xffff;

    WORD ed = x1 - x0; // Positive in east direction
    UWORD sd = y1 - y0; // Positive in south direction, guaranteed to be positive
    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;
    if (ed + sd < 0) {
        // Octant 4
        maj_d = -ed;
        min_d = sd;
        bltcon1 = SUD | AUL | ONEDOT | LINEMODE;
    } else {
        // Octant 0567 Southeast
        if (ed - sd < 0) {
            // South predominant
            maj_d = sd;
            if (ed < 0) {
                // Octant 5
                min_d = -ed;
                bltcon1 = SUL | LINEMODE; // ONEDOT doesn't actually do anything for this octant
            } else {
                // Octant 6
                min_d = ed;
                bltcon1 = LINEMODE; // ONEDOT doesn't actually do anything for this octant
            }
        } else {
            // East predominant
            // Octant 7
            maj_d = ed;
            min_d = sd;
            bltcon1 = SUD | ONEDOT | LINEMODE;
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d

    WORD bltbmod = min_d << 2; // 4min_d
    *copPtr++ = offsetof(struct Custom, bltbmod);
    *copPtr++ = bltbmod;

    maj_d <<= 2;

    WORD bltamod = bltbmod - maj_d; // 4 min_d - 4 maj_d
    WORD bltaptl = bltamod; // This goes against HRM, but seems to work well.
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltamod), bltamod);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltapt_l), bltaptl);

    if (bltaptl < 0) bltcon1 |= SIGNFLAG;
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltcon0), bltcon1);

    // Set starting word, DMA channels and logic function
    // TODO: Does this skip the first pixel? BC0F_DEST is not set.
    // See https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // https://eab.abime.net/showpost.php?p=206412&postcount=6
    //
    // FIXME: This appears to draw the first pixel, but does not draw the last pixel. 
    UWORD bltcon0 = (
        (x0 & 0xf) << 12 // Starting bit within word
        | BC0F_SRCC | BC0F_SRCA // Missing DEST here
        | ABNC | NABC | NANBC // 4a xor
    );

    // Set up
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltcon0), bltcon0);

    // Set size and start blit
    copPtr = copWrite(copPtr, offsetof(struct Custom, bltsize), (maj_d << 4) + 2);
    return copPtr;
}

__attribute__((always_inline)) inline 
WORD* cl_fill(void *bitplane, void *bitplane2, WORD* copPtr) {
    APTR start = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    APTR start2 = bitplane2 + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    WORD copperdata[] = {
        0x0001, 0x0000,
        offsetof(struct Custom, bltcon0), BC0F_SRCA | BC0F_DEST | A_TO_D,
        offsetof(struct Custom, bltcon1), FILL_XOR | BLITREVERSE,
        offsetof(struct Custom, bltafwm), 0xffff,
        offsetof(struct Custom, bltalwm), 0xffff,
        offsetof(struct Custom, bltamod), 0,
        offsetof(struct Custom, bltdmod), 0,
        offsetof(struct Custom, bltapt),
    };
    memcpy(copPtr, copperdata, sizeof(copperdata));
    copPtr += sizeof(copperdata);
    *copPtr++ = (int)start >> 16;
    *copPtr++ = offsetof(struct Custom, bltapt) + 2;
    *copPtr++ = (int)start & 0xffff;
    *copPtr++ = offsetof(struct Custom, bltdpt);
    *copPtr++ = (int)start2 >> 16;
    *copPtr++ = offsetof(struct Custom, bltdpt) + 2;
    *copPtr++ = (int)start2 & 0xffff;
    *copPtr++ = offsetof(struct Custom, bltsize);
    *copPtr++ = (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1);
    return copPtr;
}

__attribute__((always_inline)) inline 
WORD* cl_fill_fix_onedot(
    WORD y0, WORD y1, void *bitplane, WORD* copPtr
) {
    // Draws from ymin (inclusive) to ymax (exclusive)
    if (y0 > y1) {
        WORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    // Skip offscreen
    if (y1 < 0) return copPtr;
    if (y0 > YMAX) return copPtr;
    // Clip to screen
    if (y0 < 0) y0 = 0;
    if (y1 > YMAX) y1 = YMAX;
    // Skip zero length lines
    if (y1 == y0) return copPtr;

    APTR startpt = (
        bitplane
        + muluw(y0, SCREEN_WIDTH_BYTES)
        + (SCREEN_WIDTH >> 3) - 2
    ); // Location of rightmost word
    UWORD maj_d = (y1 - y0) << 1;
    WORD bltaptl = -maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d

    *copPtr++ = bltamod;
    *copPtr++ = offsetof(struct Custom, bltapt_l);
    *copPtr++ = bltaptl;
    *copPtr++ = offsetof(struct Custom, bltcpt);
    *copPtr++ = (int)startpt >> 16;
    *copPtr++ = offsetof(struct Custom, bltcpt) + 2;
    *copPtr++ = (int)startpt & 0xffff;
    *copPtr++ = offsetof(struct Custom, bltdpt);
    *copPtr++ = (int)startpt >> 16;
    *copPtr++ = offsetof(struct Custom, bltdpt) + 2;
    *copPtr++ = (int)startpt & 0xffff;
    *copPtr++ = offsetof(struct Custom, bltsize);
    *copPtr++ = (maj_d << 5) + 2;
    return copPtr;
}

__attribute__((always_inline)) inline
WORD* cl_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane, WORD* copPtr
) {
    // Draws from x0/y0 to x1/y1. Inclusive of lowest y, exclusive of largest y.
    WORD outside_viewport = 4;
    WORD viewport_intersection = 0;
    WORD mxy = 0;
    WORD myx = 0;

    // Clip at y=0
    if (y0 > y1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (y1 < 0) {
        // Entire line is above screen. Discard.
        return copPtr;
    } else if (y0 < 0) {
        // Test for intersection with top of screen
        #ifdef ASM_OPT
        mxy = x1 - x0;
        WORD yd = y1 - y0;
        asm(
            "ext.l  %[mxy]\n"
            "asl.l  %[fracbits],%[mxy]\n"
            "divs.w %[yd],%[mxy]\n"
            : [mxy]"+&d"(mxy)
            : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y0]"d"(y0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x0 - result;
        #else
        mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
        WORD new_x = x0 - ((y0 * mxy) >> FRACBITS);
        #endif
        
        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects top of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = new_x;
            y0 = 0;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at y=YMAX
    if (y0 > YMAX) {
        // Entire line is below screen. Discard.
        return copPtr;
    } else if (y1 > YMAX) {
        if (!mxy) {
            #ifdef ASM_OPT
            mxy = x1 - x0;
            WORD yd = y1 - y0;
            asm(
                "ext.l  %[mxy]\n"
                "asl.l  %[fracbits],%[mxy]\n"
                "divs.w %[yd],%[mxy]\n"
                : [mxy]"+&d"(mxy)
                : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
            #else
            mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
            #endif
        }

        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y1]"d"(YMAX - y1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x1 + result;
        #else
        WORD new_x = x1 + (((YMAX - y1) * mxy) >> FRACBITS);
        #endif

        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects bottom of screen. Move x1/y1 point onscreen and flag the intersection.
            x1 = new_x;
            y1 = YMAX;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at x=0
    if (x0 > x1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (x1 < 0) {
        // Entire line is left of screen. Discard.
        return copPtr;
    } else if (x0 < 0) {
        #ifdef ASM_OPT
        myx = y1 - y0;
        WORD xd = x1 - x0;
        asm(
            "ext.l  %[myx]\n"
            "asl.l  %[fracbits],%[myx]\n"
            "divs.w %[xd],%[myx]\n"
            : [myx]"+&d"(myx)
            : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x0]"d"(x0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y0 - result;
        #else
        myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
        WORD new_y = y0 - ((x0 * myx) >> FRACBITS);
        #endif
        
        if (new_y >= 0 && new_y <= YMAX) {
            // Line intersects left of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = 0;
            y0 = new_y;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }
    if (x0 > XMAX) {
        // Entire line is right of screen. But still need to get fill state correct.
        return cl_fill_fix_onedot(y0, y1, bitplane, copPtr);
    } else if (x1 > XMAX) {
        if (!myx) {
            #ifdef ASM_OPT
            myx = y1 - y0;
            WORD xd = x1 - x0;
            asm(
                "ext.l  %[myx]\n"
                "asl.l  %[fracbits],%[myx]\n"
                "divs.w %[xd],%[myx]\n"
                : [myx]"+&d"(myx)
                : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
            #else
            myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
            #endif
        }
        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x1]"d"(XMAX - x1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y1 + result;
        #else
        WORD new_y = y1 + (((XMAX - x1) * myx) >> FRACBITS);
        #endif
        
        if (new_y < 0) {
            // TODO: Is this needed?
            copPtr = cl_fill_fix_onedot(0, y1, bitplane, copPtr);
        } else if (new_y > YMAX) {
            // TODO: Is this needed?
            copPtr = cl_fill_fix_onedot(y1, YMAX, bitplane, copPtr);
        } else {
            copPtr = cl_fill_fix_onedot(y1, new_y, bitplane, copPtr);
            x1 = XMAX;
            y1 = new_y;
            viewport_intersection = 1;
        }
        // TODO: What kind of lines are in the else clause here? Do they also need a fillfix?
        // They cross the x=XMAX line, but not on-screen.
    } else {
        outside_viewport -= 1;
    }
    return (outside_viewport == 0 || viewport_intersection) ? cl_line_onedot(x0, y0, x1, y1, bitplane, copPtr) : copPtr;
}

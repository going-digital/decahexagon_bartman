// Project headers first: system.h -> hw.h -> custom2.h claims the
// <hardware/custom.h> include guard before any <proto/*> header can pull the
// SDK copy, keeping struct Custom identical across every translation unit.
#include "config.h"
#include "system.h"
#include "coplist.h"
#include "blitter.h"
#include "trig.h"
#include "audio.h"
#include "input.h"
#include "game.h"
#include "render.h"

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include "include/pt.h"

UWORD *bitplane_fg1;
UWORD *bitplane_fg2;
UWORD *bitplane_fg3;

static void Wait10() { WaitLine(0x10); }
static void Wait11() { WaitLine(0x11); }
static void Wait12() { WaitLine(0x12); }
static void Wait13() { WaitLine(0x13); }

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

int main() {

    SysBase = *((struct ExecBase**)4UL);

    // We will use the graphics library only to locate and restore the system copper list once we are through.
    GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR)"graphics.library",0);
    if (!GfxBase) Exit(0);

    // Used for printing
    DOSBase = (struct DosLibrary*)OpenLibrary((CONST_STRPTR)"dos.library", 0);
    if (!DOSBase) Exit(0);

#if BUILD_DEBUG
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
#endif // BUILD_DEBUG

    Delay(50);

#if BUILD_DEBUG
    warpmode(1); // fast-forward the emulator through precalc
#endif

    // Generate sin table
    init_tables();

    #ifdef MUSIC
    if(p61Init(module) != 0)
        KPrintF("p61Init failed!\n");
    #endif
    #ifdef MUSIC_LSP
    p61Init();
    #endif

#if BUILD_DEBUG
    warpmode(0);
#endif

    TakeSystem();
    WaitVbl();

    // Allocate bitplanes
    bitplane_fg1 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg2 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);
    bitplane_fg3 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP);

    // MEMF_CLEAR: the loop clobbers the copjmp2 tail with palette writes and
    // relies on the rest of the list being zero (harmless copper NOPs).
    // TODO Phase 2: rebuild the copper list properly (needs it for the wedges).
    USHORT* copper1 = (USHORT*)AllocMem(1024, MEMF_CHIP | MEMF_CLEAR);
    USHORT* copPtr = copper1;

#if BUILD_DEBUG
    // Register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_bitmap(bitplane_fg1, "FG1", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg2, "FG2", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_bitmap(bitplane_fg3, "FG3", SCREEN_WIDTH, SCREEN_HEIGHT, 1, 0);
    debug_register_copperlist(copper1, "copper1", 1024, 0);
    debug_register_copperlist(copper2, "copper2", sizeof(copper2), 0);
#endif

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

    input_init();
    game_init();
    InputState input;

    for (;;) {
        Wait10();

        // --- poll -> update -------------------------------------------------
        input_poll(&input);
        if (input.quit) break;                                  // dev: both mouse buttons
        if (input.back_edge && game_mode() == MODE_ATTRACT) break; // Escape from title quits
        game_update(&input);

        // --- render ------------------------------------------------------
        render_game(bitplane_fg2);

        #ifndef SKIP_FILL
        blit_fill(bitplane_fg2, bitplane_fg2);
        #endif
        cpu_cls(bitplane_fg3);

        // Flip render buffers on next frame
        copPtr = copWritePtr(copListSetBpl, offsetof(struct Custom, bplpt[0]), bitplane_fg2);
        #ifdef SHOW_DRAW_PLANE
        copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg3);
        #endif

        // Placeholder colour cycle (Phase 2 replaces this with per-level
        // palettes + beat pulse). White flash on the first few DEAD ticks.
        UWORD col0, col1;
        if (game_mode() == MODE_DEAD && game_mode_timer() < 6) {
            col0 = 0xfff;
            col1 = 0x000;
        } else {
            UWORD pa = frameCounter & 0x3ff;
            UWORD r = 8 + ((sin_table[pa] * 7) >> 14);
            UWORD g = 8 + ((sin_table[(pa + 1024 / 3) & 0x3ff] * 7) >> 14);
            UWORD b = 8 + ((sin_table[(pa + 2 * 1024 / 3) & 0x3ff] * 7) >> 14);
            col1 = ((r & 0xf) << 8) + ((g & 0xf) << 4) + (b & 0xf);
            col0 = (col1 >> 1) & 0x777;
        }
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[0]), col0);
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[1]), col1);

        // Bitplane fg3: Blank bitplane
        // Bitplane fg2: Line rendering and fill
        // Bitplane fg1: Display

        void* tmp = bitplane_fg1;
        bitplane_fg1 = bitplane_fg2;
        bitplane_fg2 = bitplane_fg3;
        bitplane_fg3 = tmp;

#if BUILD_DEBUG
        custom->color[0] = 0x800; // raster bar: marks where CPU work for the frame ends
#endif
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

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
    // BLITHOG gives the blitter every bus cycle - great when the CPU has
    // nothing to do during a blit, bad when it's spinning in blit_wait() while
    // real work waits. Build with -DNO_BLITHOG to let the CPU interleave.
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER
#ifndef NO_BLITHOG
        | DMAF_BLITHOG
#endif
        ;

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
    short last_frame = frameCounter;

    for (;;) {
        // Wait for the next vblank. If a frame was missed, frameCounter has
        // already moved on and we fall straight through - degrading to a lower
        // frame rate instead of the whole-frame stall Wait10() caused when
        // work crept past raster line 16.
        while (frameCounter == last_frame) {}
        // white marker: if in sync it sits at a fixed line near the top;
        // if it crawls down the screen, the loop body is over one frame.
        short missed = (short)(frameCounter - last_frame - 1);
        last_frame = frameCounter;
#if BUILD_DEBUG
        custom->color[0] = missed > 0 ? 0xf00 : 0xfff;
#endif

        // --- poll -> update -------------------------------------------------
        input_poll(&input);
        if (input.quit) break;                                  // dev: both mouse buttons
        if (input.back_edge && game_mode() == MODE_ATTRACT) break; // Escape from title quits
        game_update(&input);
#if BUILD_DEBUG
        custom->color[0] = 0xf0f; // after input+update
#endif

        // --- render ------------------------------------------------------
        // BUILD_DEBUG stacks a raster-time bar down the screen:
        // white=frame start, magenta=input+update, green=seeds, blue=fill, red=spokes.
        // Whatever colour reaches the screen bottom is where the frame ran out.
        render_game(bitplane_fg2);
#if BUILD_DEBUG
        custom->color[0] = 0x0f0;
#endif
        #ifndef SKIP_FILL
        blit_fill(bitplane_fg2, bitplane_fg2);
        #endif
#if BUILD_DEBUG
        custom->color[0] = 0x00f;
#endif
        render_spokes(bitplane_fg2); // radial slot lines, drawn over the fill
#if BUILD_DEBUG
        custom->color[0] = 0xf00;
#endif
        // Clear next frame's draw buffer with the blitter (async): it overlaps
        // the copper writes + Wait10 + next frame's input/update, so it's
        // effectively free. (cpu_cls was ~2.5ms of blocking CPU time.)
        blit_cls(bitplane_fg3);

        // Flip render buffers on next frame
        copPtr = copWritePtr(copListSetBpl, offsetof(struct Custom, bplpt[0]), bitplane_fg2);
        #ifdef SHOW_DRAW_PLANE
        copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bitplane_fg3);
        #endif

        // Fixed 2-colour palette (Phase 2 will grow this to per-level palettes
        // once there's a 2nd bitplane). Foreground brightens on the beat;
        // full white/black flash for the first few DEAD ticks.
        UWORD col0, col1;
        if (game_mode() == MODE_DEAD && game_mode_timer() < 6) {
            col0 = 0xfff;
            col1 = 0x000;
        } else {
            col0 = 0x102;                          // background: near-black blue
            col1 = game_on_beat() ? 0xfec : 0xf83; // foreground: warm orange, beat pop
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

        // No blit_wait() here: the blit_cls above runs on into Wait10 / next
        // frame's update, and render_game's blit_line_mode() waits for it
        // before anything draws into the buffer.
    }

#ifdef MUSIC
    p61End();
#endif

    // END
    FreeSystem();

    CloseLibrary((struct Library*)DOSBase);
    CloseLibrary((struct Library*)GfxBase);
}

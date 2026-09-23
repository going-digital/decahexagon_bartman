// Project headers first: system.h -> hw.h -> custom2.h claims the
// <hardware/custom.h> include guard before any <proto/*> header can pull the
// SDK copy, keeping struct Custom identical across every translation unit.
#include "config.h"
#include "pc_core.h"
#if MUSIC_FIB_STREAM
#include "tests/fib_stream.h"
#endif
#if MUSIC_FIB_BENCH
#include "tests/fib_bench.h"
#endif
#if PC_CORE_SELFTEST
#include "tests/core_checks.h"
#endif
#include "system.h"
#include "coplist.h"
#include "blitter.h"
#include "trig.h"
#include "input.h"
#include "game.h"
#include "sfx.h"
#include "paula_irq.h"
#include "render.h"
#include "hud.h"

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

// Rebuilds the copper list's per-frame-varying tail (bitplane pointers, HUD
// sprite pointers/colours, palette), then unconditionally jumps to copper2 -
// parking the copper there for the rest of the frame. Used both to seed the
// very first frame (before the main loop starts) and every loop iteration
// after: the SAME code path both times, so the list is always well-formed
// (properly terminated) rather than relying on each frame's write being at
// least as long as the previous one's. (It used not to be: the old
// steady-state write was 2 words LONGER than the one-off initial write that
// carried the copjmp2 jump, so that jump was gone from frame 1 onward and
// the copper ran off the end of this list into whatever chip memory happened
// to follow it - harmless only by luck.)
static USHORT* build_frame_tail(USHORT* copPtr, void* bpl0, void* bpl1, UWORD col0, UWORD col1) {
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[0]), bpl0);
    #ifdef SHOW_DRAW_PLANE
    copPtr = copWritePtr(copPtr, offsetof(struct Custom, bplpt[1]), bpl1);
    #else
    (void)bpl1;
    #endif

    // Set scene colours before the HUD's mid-display multiplex WAIT.
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[0]), col0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[1]), col1);
    copPtr = copWrite(copPtr, offsetof(struct Custom, color[31]), col1);

    // HUD/title pointers include a WAIT between their two sprite rows.
    copPtr = hud_emit_copper(copPtr);

    *copPtr++ = offsetof(struct Custom, copjmp2);
    *copPtr++ = 0x7fff;
    return copPtr;
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

int main() {
    int exit_status = 0;

    SysBase = *((struct ExecBase**)4UL);

    // Used for printing
    DOSBase = (struct DosLibrary*)OpenLibrary((CONST_STRPTR)"dos.library", 0);
    if (!DOSBase) Exit(0);

    // We will use the graphics library only to locate and restore the system copper list once we are through.
    GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR)"graphics.library",0);
    if (!GfxBase) Exit(0);

    video_select((GfxBase->DisplayFlags & PAL) != 0);

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
    render_init();

#if BUILD_DEBUG
    warpmode(0);
#endif

    TakeSystem();
#if SOUND_EFFECTS || MUSIC_FIB_STREAM
    paula_irq_init();
#endif
    sfx_init();
    WaitVbl();

    // Allocate bitplanes
    bitplane_fg1 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP | MEMF_CLEAR);
    bitplane_fg2 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP | MEMF_CLEAR);
    bitplane_fg3 = (UWORD*)AllocMem(BITPLANE_SIZE, MEMF_CHIP | MEMF_CLEAR);

    USHORT* copper1 = (USHORT*)AllocMem(2048, MEMF_CHIP | MEMF_CLEAR);
    if (!bitplane_fg1 || !bitplane_fg2 || !bitplane_fg3 || !copper1) {
        exit_status = 20;
        goto shutdown;
    }

    // Builds the HUD's sprite buffers - needs to happen before the copper
    // list below, which points SPRxPT at them from the very first frame.
    hud_init();

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
    // Put both playfield priority groups behind all four sprite pairs.
    // Single-playfield mode also needs PF2P set: PF1P alone does not
    // keep the HUD above the bitplane. BPLCON2 = $0024.
    copPtr = copWrite(copPtr, offsetof(struct Custom, bplcon2),
        4 * BPLCON2F_PF1P210 | 4 * BPLCON2F_PF2P210);

    // Set bitplane modulo
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl1mod), 0);
    copPtr = copWrite(copPtr, offsetof(struct Custom, bpl2mod), 0);

    // Start with a cleared display. Each list has the same static prefix.
    unsigned tail_offset = copPtr - copper1;
    copPtr = build_frame_tail(copPtr, bitplane_fg1, bitplane_fg2, 0x000, 0x000);
    USHORT* draw_copper = copper1 + 512;
    for (unsigned i = 0; i < (unsigned)(copPtr-copper1); ++i)
        draw_copper[i] = copper1[i];

    custom->cop1lc = (ULONG)copper1;
    custom->cop2lc = (ULONG)copper2;
    custom->dmacon = DMAF_BLITTER; // Disable blitter dma for copjmp bug
    custom->copjmp1 = 0x7fff; // Start copper
    // BLITHOG gives the blitter every bus cycle - great when the CPU has
    // nothing to do during a blit, bad when it's spinning in blit_wait() while
    // real work waits. Build with -DNO_BLITHOG to let the CPU interleave.
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER | DMAF_SPRITE
#ifndef NO_BLITHOG
        | DMAF_BLITHOG
#endif
        ;

    // DEMO
    SetInterruptHandler((APTR)interruptHandler);
    custom->intena = INTF_SETCLR | INTF_INTEN | INTF_VERTB;

    custom->intreq = (1 << INTB_VERTB); // Reset vbl req

    input_init();
    game_init();
    InputState input;
    UWORD last_frame = (UWORD)frameCounter;
    PcClock simulation_clock = {0};
#if PC_CORE_SELFTEST
    unsigned core_failure = pc_core_checks();
#endif

#if MUSIC_FIB_BENCH
    fib_bench_run();
#endif
    for (;;) {
        // Do not recycle either the old copper list or its bitplanes/sprites
        // until VBlank has actually installed the completed replacement.
        WaitDisplayList();
        // Wait for the next vblank. If a frame was missed, frameCounter has
        // already moved on and we fall straight through - degrading to a lower
        // frame rate instead of the whole-frame stall Wait10() caused when
        // work crept past raster line 16.
        while ((UWORD)frameCounter == last_frame) {}
        // white marker: if in sync it sits at a fixed line near the top;
        // if it crawls down the screen, the loop body is over one frame.
        UWORD now = (UWORD)frameCounter;
        UWORD elapsed_frames = (UWORD)(now - last_frame);
        UWORD missed = elapsed_frames - 1;
#if MUSIC_FIB_STREAM
        fib_stream_frame(elapsed_frames);
#endif
        last_frame = now;
#if BUILD_DEBUG
        custom->color[0] = missed > 0 ? 0x300 : 0x333;
#endif

#if MUSIC_FIB_STREAM && FIB_TRIAL_MAINLOOP
        fib_stream_fill();
#endif
        // --- poll -> update -------------------------------------------------
        input_poll(&input);
        if (input.back_edge && game_mode() == MODE_ATTRACT) break; // Escape from title quits
        ULONG ticks = pc_clock_advance(&simulation_clock, elapsed_frames, DISPLAY_RATE);
        while (ticks--) {
            game_update(&input);
#if MUSIC_FIB_STREAM
            fib_stream_tick(game_mode()==MODE_PLAYING,game_mode()==MODE_ATTRACT);
#endif
            // Held state persists; one-shot actions belong to only the first tick.
            input.fire_edge = input.back_edge = 0;
        }
        hud_tick(); // Only the final simulated state is displayed.
#if BUILD_DEBUG
        custom->color[0] = 0x303; // after input+update
#endif

        // --- render ------------------------------------------------------
        // BUILD_DEBUG stacks a raster-time bar down the screen:
        // white=frame start, magenta=input+update, green=seeds, blue=fill, red=spokes.
        // Whatever colour reaches the screen bottom is where the frame ran out.
        render_game(bitplane_fg2);
#if BUILD_DEBUG
        custom->color[0] = 0x030;
#endif
        #ifndef SKIP_FILL
        blit_fill(bitplane_fg2, bitplane_fg2);
        #endif
#if BUILD_DEBUG
        custom->color[0] = 0x003;
#endif
        render_spokes(bitplane_fg2); // radial slot lines, drawn over the fill
        render_player(bitplane_fg2); // solid sprite overlay, independently of fill
#if BUILD_DEBUG
        custom->color[0] = 0x300;
#endif
#if PC_CORE_SELFTEST
        // Visible target-executed test result: PASS or FAIL at top left.
        // This overlay is excluded from normal builds.
        // Finish DMA before the CPU modifies the same draw buffer.
        blit_wait();
        static const UBYTE pass[4][5] = {{14,9,14,8,8},{6,9,15,9,9},{7,8,6,1,14},{7,8,6,1,14}};
        static const UBYTE fail[4][5] = {{15,8,14,8,8},{6,9,15,9,9},{14,4,4,4,14},{8,8,8,8,15}};
        const UBYTE (*letters)[5] = (core_failure || game_live_failure()) ? fail : pass;
        for (WORD ch = 0; ch < 4; ++ch)
            for (WORD y = 0; y < 10; ++y)
                for (WORD x = 0; x < 8; ++x) {
                    WORD px = 8 + ch * 10 + x;
                    UBYTE *pixel = (UBYTE*)bitplane_fg2 + (26+y)*SCREEN_WIDTH_BYTES + (px>>3);
                    UBYTE mask = 0x80 >> (px & 7);
                    if (letters[ch][y/2] & (8 >> (x/2))) *pixel |= mask;
                    else *pixel &= (UBYTE)~mask;
                }
#endif

#if MUSIC_FIB_BENCH
        blit_wait();
        fib_bench_draw((UBYTE*)bitplane_fg2);
#endif
#if MUSIC_FIB_STREAM && AUDIO_DIAGNOSTICS
        blit_wait();
        fib_stream_draw((UBYTE*)bitplane_fg2);
#endif
#if CHEAT_MODE
        // Small 8 at the upper right confirms that held-key assist is active.
        if (input.cheat_held) {
            static const UBYTE cheat_indicator[5]={0x3c,0x66,0x3c,0x66,0x3c};
            blit_wait();
            for (WORD y=0;y<10;++y)
                ((UBYTE*)bitplane_fg2)[(26+y)*SCREEN_WIDTH_BYTES+SCREEN_WIDTH_BYTES-2]=cheat_indicator[y/2];
        }
#endif

        // Clear the spare buffer asynchronously. blit_cls first waits for
        // all rendering of the completed frame to finish.
        blit_cls(bitplane_fg3);

        // PC background / main-wall palette, quantized to the OCS DAC.
        // All solid geometry shares the foreground in this one-bitplane port.
        UWORD col0=pc_palette_colour(&game_palette,0);
        UWORD col1=pc_palette_colour(&game_palette,1);
        if ((game_mode()==MODE_DEAD && game_mode_timer()<6) || hud_flash_now()) {
            // PC flashlight sets every live scene colour to white.
            col0=col1=0xfff;
        }

        // Build only the inactive list, then publish it as a complete frame.
        copPtr = build_frame_tail(draw_copper + tail_offset, bitplane_fg2, bitplane_fg3, col0, col1);
        // The VBlank COPJMP workaround briefly masks blitter DMA. Publish
        // only once the spare clear is finished, so no blit is interrupted.
        blit_wait();
        QueueDisplayList(draw_copper);
        draw_copper = draw_copper == copper1 ? copper1 + 512 : copper1;

        // Bitplane fg3: Blank bitplane
        // Bitplane fg2: Line rendering and fill
        // Bitplane fg1: Display

        void* tmp = bitplane_fg1;
        bitplane_fg1 = bitplane_fg2;
        bitplane_fg2 = bitplane_fg3;
        bitplane_fg3 = tmp;

    }

shutdown:
#if MUSIC_FIB_STREAM
    fib_stream_stop();
#endif

    sfx_shutdown();
#if SOUND_EFFECTS || MUSIC_FIB_STREAM
    paula_irq_shutdown();
#endif
    // END
    FreeSystem();
    hud_free();
    render_free();
    if (bitplane_fg1) FreeMem(bitplane_fg1, BITPLANE_SIZE);
    if (bitplane_fg2) FreeMem(bitplane_fg2, BITPLANE_SIZE);
    if (bitplane_fg3) FreeMem(bitplane_fg3, BITPLANE_SIZE);
    if (copper1) FreeMem(copper1, 2048);

    if (exit_status) {
        static const char message[] = "Not enough Chip RAM for display buffers.\n";
        Write(Output(), (APTR)message, sizeof(message)-1);
    }

    CloseLibrary((struct Library*)DOSBase);
    CloseLibrary((struct Library*)GfxBase);
    return exit_status;
}

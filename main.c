// Project headers first: system.h -> hw.h -> custom2.h claims the
// <hardware/custom.h> include guard before any <proto/*> header can pull the
// SDK copy, keeping struct Custom identical across every translation unit.
#include "config.h"
#include "system.h"
#include "coplist.h"
#include "blitter.h"
#include "trig.h"
#include "audio.h"
#include "game.h"

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

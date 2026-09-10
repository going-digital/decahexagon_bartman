#include "audio.h"
#include "system.h"

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

int p61Init(void) {
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

int p61Music(void) {
    // Not needed for LSP CIA
}

int p61End(void) {
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

    void p61Music(void) {
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

    void p61End(void) {
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

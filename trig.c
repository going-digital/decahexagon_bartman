#include "trig.h"

WORD sin_table[1024];

WORD frame_sin[MAX_NUM_SIDES];
WORD frame_cos[MAX_NUM_SIDES];

void init_tables(void) {
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

#include "trig.h"
#include "view_scale.h"

WORD sin_table[1024];
WORD cos_table[1024];

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
    // Build X before modifying the raw sine samples needed by its quarter turn.
    for (UWORD i=0;i<1024;++i)
        cos_table[i]=view_scale_x(sin_table[(i+256)&1023]);
    for (UWORD i=0;i<1024;++i)
        sin_table[i]=view_scale_y(sin_table[i]);
}

void direction_to_cartesian(WORD sine, WORD cosine, UWORD length, WORD* x, WORD* y) {
    WORD result = sine;
    //result = (result * length) >> 14;
    asm(
        "muls.w %[length],%[result]\n"
        "lsl.l #2,%[result]\n"
        "swap %[result]\n"
        : [result]"+&d"(result)
        : [length]"d"(length)
        : "cc"
    );
    *y = result;

    result = cosine;
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

void polar_to_cartesian(UWORD angle, UWORD length, WORD* x, WORD* y) {
    UWORD index = angle >> 6;
    direction_to_cartesian(sin_table[index], cos_table[index], length, x, y);
}

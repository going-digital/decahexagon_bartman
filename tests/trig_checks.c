/* Target-only check: the renderer's multiplication uses 68000 instructions. */
#ifdef __m68k__
#include "../trig.h"

unsigned pc_trig_checks(void) {
    for (ULONG angle=0;angle<65536;++angle) {
        UWORD index=angle>>6;
        UWORD radius=(angle*37u)&2047;
        WORD sine=sin_table[index],cosine=sin_table[(index+256)&1023];
        WORD x,y,px,py;
        direction_to_cartesian(sine,cosine,radius,&x,&y);
        /* Independent Q14 formula, including truncation before Y correction. */
        WORD expected_x=(WORD)(((LONG)cosine*radius)>>14);
        WORD expected_y=(WORD)(((LONG)sine*radius)>>14);
        expected_y-=expected_y>>2;
        if (x!=expected_x || y!=expected_y) return 4101;
        polar_to_cartesian((UWORD)angle,radius,&px,&py);
        if (x!=px || y!=py) return 4102;
    }
    return 0;
}
#endif

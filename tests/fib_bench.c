#include "../system.h"
#include "../config.h"
#include "../fib_decode.h"
#include "fib_bench.h"
/* Adversarial stream exercises all codes, nibble order, signed wrapping,
 * independent seeds and every possible block length, without music assets. */
static UBYTE packed[257], decoded[514];
static unsigned failure, worst;
static UWORD beam(void) {
    /* A 68000 long read is two bus transfers: explicitly retry if bit 8
     * changes across the low-byte read (line 255/256 or frame rollover). */
    UWORD hi, lo, again;
    do {
        hi=custom->vposr;
        lo=custom->vhposr;
        again=custom->vposr;
    } while ((hi^again)&1);
    return ((hi&1)<<8)|(lo>>8);
}
void fib_bench_run(void) {
    static const signed char delta[16]={-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21};
    for (unsigned i=0;i<257;++i) packed[i]=(UBYTE)i;
    for (unsigned n=1;n<=512;++n) {
        decoded[0]=0xa5; decoded[n+1]=0x5a;
        fib_decode_block(packed,decoded+1,n);
        UBYTE p=packed[0];
        if(decoded[1]!=p || decoded[0]!=0xa5 || decoded[n+1]!=0x5a) failure=1;
        for(unsigned i=1;i<n;++i) {
            unsigned c=packed[1+(i-1)/2];
            p+=delta[(i&1)?c>>4:c&15];
            if(decoded[i+1]!=p) failure=1;
        }
    }
    for(unsigned i=0;i<128;++i) {
        UWORD frame=(UWORD)frameCounter;
        UWORD start=beam();
        fib_decode_block(packed,decoded,512);
        UWORD end=beam();
        /* Reject timings that may have wrapped more than one raster. */
        if((UWORD)((UWORD)frameCounter-frame)>1) failure=1;
        unsigned lines=video_timing.lines;
        unsigned elapsed=(end+lines-start)%lines+1;
        if(elapsed>worst) worst=elapsed;
    }
}
void fib_bench_draw(unsigned char *plane) {
    static const UBYTE glyphs[12][5]={
      {6,9,9,9,6},{2,6,2,2,7},{14,1,6,8,15},{14,1,6,1,14},
      {9,9,15,1,1},{15,8,14,1,14},{7,8,14,9,6},{15,1,2,4,4},
      {6,9,6,9,6},{6,9,7,1,14},{14,9,14,8,8},{15,8,14,8,8}};
    unsigned text[4]={failure?11:10,(worst/100)%10,(worst/10)%10,worst%10};
    for(unsigned ch=0;ch<4;++ch)
      for(unsigned y=0;y<10;++y)
        for(unsigned x=0;x<8;++x) {
          unsigned px=8+ch*10+x;
          UBYTE *p=plane+(26+y)*SCREEN_WIDTH_BYTES+(px>>3), mask=0x80>>(px&7);
          if(glyphs[text[ch]][y/2]&(8>>(x/2))) *p|=mask; else *p&=~mask;
        }
}

#pragma once
/* Word/long transfers require BOTH pointers aligned on 68000. An odd slice
 * length can leave them at different parity despite aligned slice starts.
 * In that case byte copying is mandatory; padding never enters the audio. */
static inline void fib_copy_samples(unsigned char *dst,const unsigned char *src,unsigned n) {
#ifdef __m68k__
    if(!(((unsigned long)dst^(unsigned long)src)&1)) {
        if(n && ((unsigned long)src&1)) {*dst++=*src++;--n;}
        while(n>=4) {
            __asm volatile("move.l (%1)+,(%0)+" : "+a"(dst), "+a"(src) : : "memory", "cc");
            n-=4;
        }
        if(n>=2) {
            __asm volatile("move.w (%1)+,(%0)+" : "+a"(dst), "+a"(src) : : "memory", "cc");
            n-=2;
        }
    }
#endif
    while(n--) *dst++=*src++;
}

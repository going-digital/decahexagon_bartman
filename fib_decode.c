#include "fib_decode.h"

/* Keep benchmark calls identical to the general runtime decoder under LTO. */
#ifdef __clang__
__attribute__((noinline))
#else
__attribute__((noinline, noclone))
#endif
void fib_decode_block(const unsigned char *src, unsigned char *dst, unsigned samples)
{
    static const signed char delta[16] = {
        -34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21
    };
    unsigned char predictor = *src++;
    *dst++ = predictor;
    unsigned remaining = samples - 1;
    while (remaining >= 2) {
        unsigned codes = *src++;
        predictor += delta[codes >> 4];
        *dst++ = predictor;
        predictor += delta[codes & 15];
        *dst++ = predictor;
        remaining -= 2;
    }
    if (remaining) {
        predictor += delta[*src >> 4];
        *dst = predictor;
    }
}

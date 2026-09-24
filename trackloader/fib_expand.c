#include "fib_expand.h"
#include "../fib_decode.h"
static unsigned le32(const unsigned char *p) {
    return (unsigned)p[0] | ((unsigned)p[1]<<8) |
           ((unsigned)p[2]<<16) | ((unsigned)p[3]<<24);
}
unsigned trackloader_fib_expand(unsigned char *arena, unsigned input_bytes,
                               unsigned capacity) {
    unsigned samples, blocks, last, expected;
    unsigned char scratch[257];
    if (input_bytes < 12 || input_bytes > capacity) return 0;
    if (arena[0]!='F' || arena[1]!='I' || arena[2]!='B' || arena[3]!='1') return 0;
    samples=le32(arena+4);
    if (!samples || samples>capacity || le32(arena+8)!=512) return 0;
    blocks=(samples-1)/512+1;
    last=samples-(blocks-1)*512;
    expected=12+(blocks-1)*257+1+last/2;
    if (expected!=input_bytes) return 0;
    /* Each block is independently seeded. Stage it before expanding backward
     * through the bank. Output cannot touch any earlier unread block:
     * output start k*512 >= input start 12+k*257 for every k >= 1.
     * Block zero has no earlier unread data; its header is already consumed. */
    while (blocks) {
        unsigned k=--blocks;
        unsigned n=k==(samples-1)/512 ? last : 512;
        unsigned bytes=1+n/2;
        const unsigned char *src=arena+12+k*257;
        for (unsigned i=0;i<bytes;++i) scratch[i]=src[i];
        fib_decode_block(scratch,arena+k*512,n);
    }
    return samples;
}

#include "../pc_time.h"
#include <assert.h>
#include <stdio.h>
static void check(uint32_t ticks) {
    uint32_t q=pc_tick_seconds(ticks);
    assert(q==ticks/60u);
    assert(ticks-q*60u==ticks%60u);
}
int main(void) {
    for(uint32_t i=0;i<65536u;++i)check(i);
    for(uint32_t i=65536;i<66000;++i)check(i);
    uint32_t rng=1;
    for(unsigned i=0;i<1000000;++i) {
        rng=rng*1664525u+1013904223u;check(rng);
    }
    check(0xffffffffu);check(3932159u);check(3932160u);
    puts("Time conversion: 1,066,003 quotient/remainder cases match");
}

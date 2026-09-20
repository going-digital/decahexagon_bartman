#include <assert.h>
#include "../pcm_lifecycle.h"
int main(void) {
    const unsigned offsets[4]={0,27477,80410,110000};
    for(unsigned r=0;r<65536;++r) {
        assert(pcm_start_offset_ms(0,r)==0);
        assert(pcm_start_offset_ms(1,r)==offsets[r&3]);
    }
    PcmLifecycle s={0};
    for(unsigned i=0;i<120;++i) assert(!pcm_lifecycle_tick(&s,0,1)&&!s.active);
    assert(pcm_lifecycle_tick(&s,1,0)==PCM_START && s.volume==0);
    for(unsigned i=1;i<=30;++i) {
        assert(!pcm_lifecycle_tick(&s,1,0));assert(s.volume==64*i/30);
    }
    assert(!pcm_lifecycle_tick(&s,0,0));
    for(unsigned i=1;i<45;++i) {
        assert(!pcm_lifecycle_tick(&s,0,0));assert(s.volume==64*(45-i)/45);
    }
    assert(pcm_lifecycle_tick(&s,0,0)==PCM_STOP && !s.active && !s.volume);
    assert(!pcm_lifecycle_tick(&s,0,0));
    assert(pcm_lifecycle_tick(&s,1,0)==PCM_START);
    assert(!pcm_lifecycle_tick(&s,0,0));
    assert(pcm_lifecycle_tick(&s,1,0)==(PCM_START|PCM_STOP));
    assert(pcm_lifecycle_tick(&s,0,1)==PCM_STOP);
    assert(!pcm_lifecycle_tick(&s,0,1));
    assert(pcm_lifecycle_tick(&s,1,0)==PCM_START);
    return 0;
}

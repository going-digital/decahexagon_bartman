#include <assert.h>
#include "../pcm_lifecycle.h"
int main(void) {
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

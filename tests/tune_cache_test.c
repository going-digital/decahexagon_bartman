#include <assert.h>
#include <string.h>
#include "trackloader/tune_cache.h"
static unsigned char banks[3][1],meta[96];
static unsigned reads,hits,fail;
static int load(unsigned profile,void *arena,const void *cached) {
    unsigned track=profile%3;
    if(fail){fail=0;return 0;}
    if(cached){assert(*(unsigned char*)arena==track);assert(((const unsigned char*)cached)[0]==track);hits++;}
    else{*(unsigned char*)arena=track;memset(meta,track,96);reads++;}
    return 1;
}
static void start(TrackTuneCache *c,unsigned count) {
    void *arenas[3]={banks[0],count>1?banks[1]:0,count>2?banks[2]:0};
    reads=hits=fail=0;track_tune_cache_init(c,arenas,meta,load);
    for(unsigned i=0;i<count;i++)assert(track_tune_cache_prepare(c,i));
    assert(reads==count);
}
int main(void) {
    TrackTuneCache c;
    start(&c,1);
    assert(track_tune_cache_prepare(&c,3) && reads==1 && hits==0);
    assert(track_tune_cache_prepare(&c,1) && reads==2);
    assert(track_tune_cache_prepare(&c,0) && reads==3);
    start(&c,2);
    assert(track_tune_cache_prepare(&c,0) && hits==1);
    assert(track_tune_cache_prepare(&c,2) && reads==3);
    assert(track_tune_cache_prepare(&c,0) && hits==2); /* recently used survived */
    assert(track_tune_cache_prepare(&c,1) && reads==4); /* 2 was evicted */
    fail=1;assert(!track_tune_cache_prepare(&c,2));
    assert(c.active==-1);
    assert(track_tune_cache_prepare(&c,2) && reads==5); /* failed load not cached */
    start(&c,3);
    for(unsigned i=0;i<600;i++)assert(track_tune_cache_prepare(&c,i%6));
    assert(reads==3 && hits==600);
    assert(!track_tune_cache_prepare(&c,6));
    return 0;
}

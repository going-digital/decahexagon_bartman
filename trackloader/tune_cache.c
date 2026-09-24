#include "tune_cache.h"
void track_tune_cache_init(TrackTuneCache *c,void *const arenas[3],void *metadata,TrackTuneLoad load) {
    c->count=0;c->active=-1;c->metadata=metadata;c->load=load;
    for(unsigned i=0;i<3;i++)if(arenas[i]) {
        TrackTuneSlot *s=&c->slots[c->count++];
        s->arena=arenas[i];s->valid=s->rank=0;
    }
}
int track_tune_cache_prepare(TrackTuneCache *c,unsigned profile) {
    if(profile>=6 || !c->count || !c->metadata || !c->load)return 0;
    unsigned track=profile%3,slot=0;
    for(;slot<c->count;slot++)if(c->slots[slot].valid && c->slots[slot].track==track)break;
    unsigned hit=slot<c->count;
    if(!hit) {
        for(slot=0;slot<c->count;slot++)if(!c->slots[slot].valid)break;
        if(slot==c->count) {
            slot=0;
            for(unsigned i=1;i<c->count;i++)if(c->slots[i].rank>c->slots[slot].rank)slot=i;
        }
    }
    TrackTuneSlot *s=&c->slots[slot];
    unsigned rank=hit?s->rank:3;
    if(!hit || c->active!=(int)slot) {
        c->active=-1;
        if(!hit)s->valid=0;
        if(!c->load(profile,s->arena,hit?s->metadata:0))return 0;
        if(!hit)for(unsigned i=0;i<96;i++)s->metadata[i]=((unsigned char*)c->metadata)[i];
        s->track=track;s->valid=1;c->active=(int)slot;
    }
    for(unsigned i=0;i<c->count;i++)if(c->slots[i].valid && c->slots[i].rank<rank)c->slots[i].rank++;
    s->rank=0;
    return 1;
}

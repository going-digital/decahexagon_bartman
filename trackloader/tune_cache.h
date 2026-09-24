#pragma once
/* Decoded tune banks remain in place; metadata contains pointers into them. */
typedef int (*TrackTuneLoad)(unsigned profile,void *arena,const void *cached_metadata);
typedef struct {
    void *arena;
    unsigned char metadata[96];
    unsigned char valid,track,rank;
} TrackTuneSlot;
typedef struct {
    TrackTuneSlot slots[3];
    unsigned count;
    int active;
    void *metadata;
    TrackTuneLoad load;
} TrackTuneCache;
void track_tune_cache_init(TrackTuneCache *cache,void *const arenas[3],void *metadata,TrackTuneLoad load);
int track_tune_cache_prepare(TrackTuneCache *cache,unsigned profile);

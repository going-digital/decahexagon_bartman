#pragma once
#include "../fib_pcm.h"
typedef struct {
    unsigned rate, samples, metadata_bytes, pcm_bytes, arena_bytes;
} TrackTuneInfo;
/* Validate an inflated TUN1/FIB1 package without changing it. */
int trackloader_tune_check(const unsigned char *p, unsigned bytes,
                          unsigned capacity, TrackTuneInfo *info);
/* Validate, expand in-place, then bind the existing PCM sequencer.
 * The caller must stop audio before replacing its bank. No gain is applied.
 * Returns zero without changing package or song on malformed input. */
int trackloader_tune_prepare(unsigned char *p, unsigned bytes, unsigned capacity,
                            PcmSong *song, TrackTuneInfo *info);

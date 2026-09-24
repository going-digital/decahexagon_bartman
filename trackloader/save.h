#pragma once
#include <stdint.h>
#include "../pc_menu.h"
#define TRACK_SAVE_BYTES 512
#define TRACK_SAVE_ID_BYTES 16
typedef struct {
    PcRecords records;
    uint32_t achievements; /* opaque until achievement definitions are verified */
    uint32_t generation;
} TrackSave;
/* Identity must come from trusted disk/build metadata. These routines only
 * serialize RAM: they do not authorize writes or establish media identity.
 * Buffers/outputs must not overlap. Failure never changes the output. */
int track_save_encode(unsigned char *slot,const unsigned char *identity,const TrackSave *state);
int track_save_decode(const unsigned char *slot,const unsigned char *identity,TrackSave *state);
/* 0/1 = selected slot; -1 = neither valid; -2 = ambiguous generations/state.
 * Wrap-safe provided committed generations differ by less than 2^31.
 * Equal generations are accepted only for byte-identical slots. */
int track_save_select(const unsigned char *a,const unsigned char *b,
                      const unsigned char *identity,TrackSave *state);

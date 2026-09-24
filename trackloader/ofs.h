#pragma once
#include <stdint.h>
/* Flat, ASCII-named OFS payload files on the native DD disk. No allocation,
 * writes, subdirectories or OS calls. Callback returns nonzero for one sector.
 * Caller owns scratch, destination and callback context as disjoint storage.
 * On failure the destination may contain a validated prefix; never publish it.
 * Expected bytes come from trusted build metadata, not the filesystem. */
typedef int (*TrackSectorRead)(void *context, uint32_t sector, unsigned char *block);
typedef struct {
    unsigned char metadata[512], data[512], seen[220];
} TrackOfsScratch;
int track_ofs_load(TrackSectorRead read, void *context, const char *name,
    unsigned char *destination, uint32_t capacity, uint32_t expected_bytes,
    TrackOfsScratch *scratch);

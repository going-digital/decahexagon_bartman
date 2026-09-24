#pragma once
#include "../game.h"
#include "tune_cache.h"
/* Resident caller: relocated game and zeroed BSS, supervisor mode with CPU
 * interrupts masked, DMA/CIAs quiesced. Code/static DMA data must be in Chip
 * memory until the final linker layout separates them. Stack, image, loader,
 * disk workspace, tune arena and chip_heap must be disjoint and remain live.
 * Native boot targets an uncached 68000. The WHDLoad caller separately owns
 * cache synchronization and OS return; neither is implied by this interface. */
typedef struct {
    void *chip_heap;
    unsigned long chip_bytes;
    void *vbr;
    unsigned pal;
    TrackTuneLoad prepare;
    const TrackSave *saved_state; /* optional decoded startup state; copied immediately */
    int (*disk_transfer)(unsigned write,unsigned sector,void *buffer);
    const unsigned char *save_identity;
    const unsigned char *save_anchors; /* three BE sector numbers then 3*512 bytes */
    void *save_scratch; /* 2048-byte, even-aligned Chip RAM transaction workspace */
    void *tune_arenas[3]; /* 500000 bytes each; allocated before takeover */
    void *tune_metadata; /* resident info + PCM binding, 96 bytes */
#if WHDLOAD
    int (*commit_save)(const TrackSave *snapshot);
#else
    unsigned boot_drive; /* physical trackdisk unit 0..3 */
#endif
} TrackGameBoot;
/* Returns with interrupts and DMA off. Caller supplies any subsequent UI/reset.
 * Nonzero indicates startup failure. Never call concurrently or recursively. */
int trackloader_game_entry(const TrackGameBoot *boot);
void TrackSystemSetVBR(void *vbr);

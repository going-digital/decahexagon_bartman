#pragma once
#include "../game.h"
/* Resident caller: relocated game and zeroed BSS, supervisor mode with CPU
 * interrupts masked, DMA/CIAs quiesced. Code/static DMA data must be in Chip
 * memory until the final linker layout separates them. Stack, image, loader,
 * disk workspace, tune arena and chip_heap must be disjoint and remain live.
 * Current target: uncached 68000 only. No OS return or cache setup is implied. */
typedef struct {
    void *chip_heap;
    unsigned long chip_bytes;
    void *vbr;
    unsigned pal;
    GameRunPreparer prepare;
    const TrackSave *saved_state; /* optional decoded startup state; copied immediately */
    int (*disk_transfer)(unsigned write,unsigned sector,void *buffer);
    const unsigned char *save_identity;
    const unsigned char *save_anchors; /* three BE sector numbers then 3*512 bytes */
    void *save_scratch; /* 2048-byte, even-aligned Chip RAM transaction workspace */
} TrackGameBoot;
/* Returns with interrupts and DMA off. Caller supplies any subsequent UI/reset.
 * Nonzero indicates startup failure. Never call concurrently or recursively. */
int trackloader_game_entry(const TrackGameBoot *boot);
void TrackSystemSetVBR(void *vbr);

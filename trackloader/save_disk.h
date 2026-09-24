#pragma once
#include "save.h"
/* Raw slots occupy separate reserved tracks, never filesystem blocks. */
#define TRACK_SAVE_SECTOR_A 1738
#define TRACK_SAVE_SECTOR_B 1749
typedef int (*TrackSaveRead)(void*,uint32_t,unsigned char*);
typedef int (*TrackSaveWrite)(void*,uint32_t,const unsigned char*);
typedef struct {
    unsigned char identity[16];
    /* Trusted immutable sector snapshots supplied by the image builder.
     * Includes root (880), bitmap and boot/identity anchor. Never populate
     * these expected bytes from the disk being checked. */
    uint32_t sector[3];
    const unsigned char *expected[3];
} TrackSaveMedia;
typedef struct { unsigned char slots[2][512],pending[512],check[512]; } TrackSaveDiskScratch;
enum { TRACK_SAVE_DISK_BLANK=2, TRACK_SAVE_DISK_OK=1, TRACK_SAVE_DISK_IO=-1,
       TRACK_SAVE_DISK_IDENTITY=-2, TRACK_SAVE_DISK_CONFLICT=-3,
       TRACK_SAVE_DISK_VERIFY=-4, TRACK_SAVE_DISK_STATE=-5 };
/* Synchronous, uncached I/O. Adapter owns motor cleanup, write protection and
 * media-change detection. Must abort if media changes during the transaction.
 * Success means verified write/readback, not merely a completed write call.
 * No game acknowledgement occurs here. Scratch and inputs must be disjoint. */
int track_save_disk_commit(TrackSaveRead read,TrackSaveWrite write,void *context,
    const TrackSaveMedia *media,const TrackSave *snapshot,TrackSaveDiskScratch *scratch);

/* Startup recovery. OK returns newest valid state, BLANK returns zero defaults.
 * All failures preserve output. Neither call writes or acknowledges gameplay. */
int track_save_disk_load(TrackSaveRead read,void *context,const TrackSaveMedia *media,
    TrackSave *state,TrackSaveDiskScratch *scratch);

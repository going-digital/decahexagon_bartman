/* Transaction-boundary faults. The callback models a latched disk change;
 * this does not test CIA timing or a removal inside a DiskIO operation. */
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "../trackloader/save_disk.h"

typedef struct { unsigned char anchors[3][512], slots[2][512]; } Disk;
static Disk original, replacement, before;
static unsigned calls, swap_at, changed, writes, replacement_writes;
static const uint32_t sectors[3]={0,880,881};
static int boundary(void) {
    if(++calls==swap_at)changed=1;
    return !changed;
}
static unsigned char *sector(Disk *disk,uint32_t n) {
    for(unsigned i=0;i<3;i++)if(n==sectors[i])return disk->anchors[i];
    assert(n==TRACK_SAVE_SECTOR_A || n==TRACK_SAVE_SECTOR_B);
    return disk->slots[n==TRACK_SAVE_SECTOR_B];
}
static int read_sector(void *ctx,uint32_t n,unsigned char *out) {
    (void)ctx;
    int ok=boundary();
    /* Even failed callbacks may leave output containing replacement data. */
    memcpy(out,sector(changed?&replacement:&original,n),512);
    return ok;
}
static int write_sector(void *ctx,uint32_t n,const unsigned char *in) {
    (void)ctx;
    if(!boundary())return 0;
    writes++;replacement_writes+=changed;
    memcpy(sector(changed?&replacement:&original,n),in,512);
    return 1;
}
static void begin(unsigned fault) { calls=changed=writes=replacement_writes=0;swap_at=fault; }
int main(void) {
    TrackSaveMedia media={{0},{0,880,881},{original.anchors[0],original.anchors[1],original.anchors[2]}};
    TrackSaveDiskScratch scratch;
    TrackSave old={0},next={0},out;
    unsigned char active[512],encoded[512];
    unsigned cases=0;
    old.generation=7;old.records.best[0]=4000;
    next=old;next.generation=8;next.records.best[0]=5000;
    assert(track_save_encode(active,media.identity,&old));
    assert(track_save_encode(encoded,media.identity,&next));
    for(unsigned orientation=0;orientation<2;orientation++)for(unsigned clone=0;clone<2;clone++) {
        memset(&original,0,sizeof original);
        memcpy(original.slots[orientation],active,512);
        before=original;
        replacement=before;
        if(!clone)memset(&replacement,0xcc,sizeof replacement);
        Disk untouched=replacement;
        begin(0);
        assert(track_save_disk_commit(read_sector,write_sector,0,&media,&next,&scratch)==TRACK_SAVE_DISK_OK);
        unsigned transfers=calls;
        assert(transfers==13 && writes==1);
        for(unsigned fault=1;fault<=transfers;fault++) {
            original=before;begin(fault);
            assert(track_save_disk_commit(read_sector,write_sector,0,&media,&next,&scratch)==TRACK_SAVE_DISK_IO);
            assert(!replacement_writes && !memcmp(&replacement,&untouched,sizeof replacement));
            assert(!memcmp(original.slots[orientation],active,512));
            assert(next.generation==8 && next.records.best[0]==5000);
            /* Reinserting the original permits retry of the same snapshot.
             * If its write already completed, retry must acknowledge without
             * rewriting or advancing the generation a second time. */
            unsigned already_written=writes;
            begin(0);
            assert(track_save_disk_commit(read_sector,write_sector,0,&media,&next,&scratch)==TRACK_SAVE_DISK_OK);
            assert(writes==!already_written);
            assert(!memcmp(original.slots[1-orientation],encoded,512));
            assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_OK);
            assert(out.generation==8 && out.records.best[0]==5000);
            cases++;
        }
        /* Never publish a mixed-media startup snapshot, either. */
        original=before;begin(0);
        assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_OK);
        transfers=calls;assert(transfers==8);
        for(unsigned fault=1;fault<=transfers;fault++) {
            begin(fault);memset(&out,0xa5,sizeof out);TrackSave sentinel=out;
            assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_IO);
            assert(!memcmp(&out,&sentinel,sizeof out));cases++;
        }
    }
    printf("Save media change: %u boundary faults pass (latched adapter model); retries preserve generation\n",cases);
    return 0;
}

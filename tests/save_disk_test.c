#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "../trackloader/save_disk.h"
static unsigned char disk[1760][512],anchors[3][512];
static unsigned writes,last_sector,fail_write,bad_readback,change_identity;
static uint32_t fail_read=0xffffffffUL;
static int read_sector(void *ctx,uint32_t n,unsigned char *b) {
    (void)ctx;assert(n<1760);if(n==fail_read)return 0;memcpy(b,disk[n],512);
    if(bad_readback && writes && n==last_sector)b[100]^=1;
    return 1;
}
static int write_sector(void *ctx,uint32_t n,const unsigned char *b) {
    (void)ctx;assert(n==1738 || n==1749);writes++;last_sector=n;
    if(fail_write)return 0;
    memcpy(disk[n],b,512);if(change_identity)disk[880][0]^=1;
    return 1;
}
int main(void) {
    TrackSaveMedia media={{0},{0,880,881},{anchors[0],anchors[1],anchors[2]}};
    TrackSaveDiskScratch scratch;TrackSave state={0},decoded={0};
    for(unsigned i=0;i<3;i++){memset(anchors[i],i+1,512);memcpy(disk[media.sector[i]],anchors[i],512);}
    decoded.generation=99;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_BLANK);
    assert(decoded.generation==0 && decoded.records.best[0]==0);
    decoded.generation=99;fail_read=1749;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_IO && decoded.generation==99);
    fail_read=0xffffffffUL;
    state.generation=1;state.records.best[0]=4000;state.records.completed[0]=1;
    disk[880][20]^=1;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_IDENTITY && !writes);
    disk[880][20]^=1;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==1 && writes==1 && last_sector==1738);
    unsigned char previous[512];memcpy(previous,disk[1738],512);
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_OK && writes==1);
    state.records.best[0]++;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_CONFLICT && writes==1);
    state.records.best[0]--;
    state.generation=2;state.records.best[0]=5000;fail_write=1;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_IO);
    assert(!memcmp(previous,disk[1738],512));fail_write=0;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==1 && last_sector==1749);
    assert(!memcmp(previous,disk[1738],512));
    assert(track_save_select(disk[1738],disk[1749],media.identity,&decoded)==1 && decoded.records.best[0]==5000);
    state.generation=3;bad_readback=1;
    /* Callback fault is limited to the actual readback of the next write. */
    writes=0;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_VERIFY);
    bad_readback=0;
    unsigned saved_writes=writes;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_OK && decoded.generation==3);
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_OK && writes==saved_writes);
    state.generation=4;change_identity=1;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_IDENTITY);
    change_identity=0;disk[880][0]^=1;
    memset(disk[1738],0,512);memset(disk[1749],0,512);disk[1738][0]=1;
    unsigned before=writes;state.generation=1;
    assert(track_save_disk_commit(read_sector,write_sector,0,&media,&state,&scratch)==TRACK_SAVE_DISK_CONFLICT && writes==before);
    decoded.generation=99;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_CONFLICT && decoded.generation==99);
    /* Cold restart after a torn newest slot falls back to the earlier one. */
    state.generation=7;assert(track_save_encode(disk[1738],media.identity,&state));
    state.generation=8;assert(track_save_encode(disk[1749],media.identity,&state));
    disk[1749][100]^=1;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_OK && decoded.generation==7);
    disk[880][0]^=1;decoded.generation=99;
    assert(track_save_disk_load(read_sector,0,&media,&decoded,&scratch)==TRACK_SAVE_DISK_IDENTITY && decoded.generation==99);
    puts("Save transaction: identity, blank initialization, inactive slots, failed writes, readback and conflicts pass");
    return 0;
}

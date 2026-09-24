#include <assert.h>
#include <string.h>
#include "whdload/save_files.h"
static unsigned char disk[2][512],id[16]={1};
static int fail_write,fail_read,reads,writes;
static int io(unsigned write,unsigned slot,void *b) {
    assert(slot<2);
    if(write){writes++;if(fail_write)return 0;memcpy(disk[slot],b,512);}
    else{reads++;if(fail_read && writes)return 0;memcpy(b,disk[slot],512);}
    return 1;
}
int main(void) {
    TrackSaveDiskScratch scratch;TrackSave s={0},restored;
    s.generation=1;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_OK);
    assert(track_save_select(disk[0],disk[1],id,&restored)==0);
    assert(restored.generation==1);
    s.generation=2;fail_write=1;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_IO);
    assert(track_save_select(disk[0],disk[1],id,&restored)==0);
    fail_write=0;writes=0;fail_read=1;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_IO);
    fail_read=0;writes=0;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_OK && writes==0);
    assert(track_save_select(disk[0],disk[1],id,&restored)==1 && restored.generation==2);
    disk[1][50]^=1;
    assert(track_save_select(disk[0],disk[1],id,&restored)==0);
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_OK);
    s.generation=7;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_CONFLICT);
    memset(disk,99,sizeof(disk));s.generation=1;
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_CONFLICT);
    memset(disk,255,sizeof(disk));s.generation=0xffffffffu;
    assert(track_save_encode(disk[0],id,&s));
    s.generation=0; /* wrap, while repairing the invalid other slot */
    assert(whd_save_commit(&s,id,&scratch,io)==TRACK_SAVE_DISK_OK);
    assert(track_save_select(disk[0],disk[1],id,&restored)==1 && restored.generation==0);
    return 0;
}

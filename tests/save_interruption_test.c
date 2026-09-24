#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "../trackloader/save_disk.h"
static unsigned char slots[2][512],anchors[3][512];
static unsigned cut,writes,target,lie;
static int failed_slot=-1;
static int read_sector(void *ctx,uint32_t n,unsigned char *out) {
    (void)ctx;
    for(unsigned i=0;i<3;i++)if(n==(unsigned[]){0,880,881}[i]){memcpy(out,anchors[i],512);return 1;}
    unsigned index=n==1738?0:1;assert(n==1738 || n==1749);
    memcpy(out,slots[index],512); /* failed read may still have modified output */
    return (int)index!=failed_slot;
}
static int write_sector(void *ctx,uint32_t n,const unsigned char *in) {
    (void)ctx;unsigned index=n==1738?0:1;assert(n==1738 || n==1749);
    assert(index==target);writes++;memcpy(slots[index],in,cut);
    return lie; /* transport can fail, or falsely report a partial write complete */
}
static void same(const TrackSave *a,const TrackSave *b) {
    assert(a->generation==b->generation && a->achievements==b->achievements);
    for(unsigned i=0;i<6;i++)assert(a->records.best[i]==b->records.best[i] && a->records.completed[i]==b->records.completed[i]);
}
int main(void) {
    TrackSaveMedia media={{0},{0,880,881},{anchors[0],anchors[1],anchors[2]}};
    TrackSaveDiskScratch scratch;TrackSave old={0},older={0},next={0},out={0};
    unsigned char committed[512],stale[512];unsigned cases=0;
    old.generation=7;old.records.best[0]=4000;old.records.completed[0]=1;old.achievements=1;
    older=old;older.generation=6;older.records.best[0]=3900;
    next=old;next.generation=8;next.records.best[0]=5000;next.achievements=3;
    assert(track_save_encode(committed,media.identity,&old));assert(track_save_encode(stale,media.identity,&older));
    for(target=0;target<2;target++)for(lie=0;lie<2;lie++)for(cut=0;cut<=512;cut++) {
        memcpy(slots[1-target],committed,512);memcpy(slots[target],stale,512);writes=0;
        int result=track_save_disk_commit(read_sector,write_sector,0,&media,&next,&scratch);
        assert(writes==1 && !memcmp(slots[1-target],committed,512));
        assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_OK);
        if(out.generation==old.generation)same(&out,&old);else same(&out,&next);
        if(result==TRACK_SAVE_DISK_OK)same(&out,&next);
        else assert(result==TRACK_SAVE_DISK_IO || result==TRACK_SAVE_DISK_VERIFY);
        cases++;
    }
    for(target=0;target<2;target++) {
        memcpy(slots[1-target],committed,512);memset(slots[target],0xcc,512);
        failed_slot=(int)target;
        assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_OK);same(&out,&old);
        writes=0;
        assert(track_save_disk_commit(read_sector,write_sector,0,&media,&next,&scratch)==TRACK_SAVE_DISK_IO && !writes);
        memset(slots[1-target],0,512);out=old;
        assert(track_save_disk_load(read_sector,0,&media,&out,&scratch)==TRACK_SAVE_DISK_IO);same(&out,&old);
    }
    printf("Save interruption: %u prefix/transport cases, both slot orientations and unreadable-track recovery pass\n",cases);
    return 0;
}

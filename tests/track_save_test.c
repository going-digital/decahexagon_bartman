#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "../trackloader/save.h"
static const unsigned char id[16]="Hexagon test id";
static unsigned char a[512],b[512],fresh[512],torn[512];
static void equal(const TrackSave *a,const TrackSave *b) {
    assert(a->generation==b->generation && a->achievements==b->achievements);
    for(unsigned i=0;i<6;i++)assert(a->records.best[i]==b->records.best[i] &&
        a->records.completed[i]==b->records.completed[i]);
}
int main(void) {
    TrackSave old={0},next={0},out={0},sentinel={0};
    old.generation=10;old.achievements=0xdeadbeef;
    for(unsigned i=0;i<6;i++){old.records.best[i]=3601+i;old.records.completed[i]=i&1;}
    assert(track_save_encode(a,id,&old));
    assert(!memcmp(a,"HXS1",4) && a[11]==0 && a[10]==2 && a[15]==10);
    assert(track_save_decode(a,id,&out));equal(&out,&old);
    sentinel.generation=123;sentinel.achievements=456;
    for(unsigned i=0;i<512;i++) {
        memcpy(torn,a,512);torn[i]^=1;out=sentinel;
        assert(!track_save_decode(torn,id,&out));equal(&out,&sentinel);
    }
    out=sentinel;assert(!track_save_decode(a,(const unsigned char*)"wrong disk id!!!",&out));equal(&out,&sentinel);
    memset(b,0,512);assert(track_save_select(a,b,id,&out)==0);equal(&out,&old);
    assert(track_save_select(b,a,id,&out)==1);equal(&out,&old);
    out=sentinel;assert(track_save_select(b,b,id,&out)==-1);equal(&out,&sentinel);
    assert(track_save_select(a,a,id,&out)==0);
    next=old;next.records.best[0]++;assert(track_save_encode(b,id,&next));
    out=sentinel;assert(track_save_select(a,b,id,&out)==-2);equal(&out,&sentinel);
    next.generation=old.generation+0x80000000UL;assert(track_save_encode(b,id,&next));
    assert(track_save_select(a,b,id,&out)==-2);equal(&out,&sentinel);
    /* Every prefix length of a torn inactive-slot replacement recovers either
       the previous committed state or the entire new state, never a mixture. */
    next=old;next.generation=11;next.records.best[5]=99999;next.achievements=7;
    assert(track_save_encode(fresh,id,&next));
    TrackSave older=old;older.generation=9;assert(track_save_encode(b,id,&older));
    for(unsigned cut=0;cut<=512;cut++) {
        memcpy(torn,b,512);memcpy(torn,fresh,cut);
        int selected=track_save_select(a,torn,id,&out);
        assert(selected==0 || selected==1);equal(&out,selected?&next:&old);
    }
    old.generation=0xffffffffUL;next.generation=0;
    assert(track_save_encode(a,id,&old) && track_save_encode(b,id,&next));
    assert(track_save_select(a,b,id,&out)==1);equal(&out,&next);
    assert(track_save_select(b,a,id,&out)==0);equal(&out,&next);
    next.records.completed[0]=2;memset(torn,0xa5,512);
    assert(!track_save_encode(torn,id,&next));for(unsigned i=0;i<512;i++)assert(torn[i]==0xa5);
    TrackSave defaults={0};
    for(unsigned i=0;i<6;i++)assert(pc_profile_unlocked(&defaults.records,i)==(i<3));
    puts("Save codec: round-trip, 512 corruptions, 513 torn writes, identity, ambiguity and wrap recovery pass");
    return 0;
}

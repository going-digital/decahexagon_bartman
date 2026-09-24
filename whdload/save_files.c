#include "save_files.h"
static int same(const unsigned char *a,const unsigned char *b) {
    for(unsigned i=0;i<512;i++)if(a[i]!=b[i])return 0;
    return 1;
}
static int blank(const TrackSaveDiskScratch *s) {
    for(unsigned i=0;i<512;i++)if(s->slots[0][i] || s->slots[1][i])return 0;
    return 1;
}
int whd_save_commit(const TrackSave *snapshot,const unsigned char *identity,
    TrackSaveDiskScratch *s,WhdSaveIO io) {
    TrackSave current;int active,target;
    if(!io || !identity || !snapshot || !s)return TRACK_SAVE_DISK_STATE;
    if(!track_save_encode(s->pending,identity,snapshot))return TRACK_SAVE_DISK_STATE;
    if(!io(0,0,s->slots[0]) ||
       !io(0,1,s->slots[1]))return TRACK_SAVE_DISK_IO;
    active=track_save_select(s->slots[0],s->slots[1],identity,&current);
    if(active==-2)return TRACK_SAVE_DISK_CONFLICT;
    if(active==-1) {
        /* Only explicitly blank slots may start a new journal automatically.
         * Two corrupt or foreign slots require an explicit recovery decision. */
        if(!blank(s))return TRACK_SAVE_DISK_CONFLICT;
        if(snapshot->generation!=1)return TRACK_SAVE_DISK_CONFLICT;
        target=0;
    } else {
        /* The previous write may have succeeded before its verification failed.
         * An exact, newly read committed snapshot is safe to acknowledge again;
         * same-generation divergent state remains a conflict. */
        if(snapshot->generation==current.generation && same(s->pending,s->slots[active]))
            return TRACK_SAVE_DISK_OK;
        if(snapshot->generation!=current.generation+1)return TRACK_SAVE_DISK_CONFLICT;
        target=1-active;
    }
    if(!io(1,target,s->pending))return TRACK_SAVE_DISK_IO;
    if(!io(0,target,s->check))return TRACK_SAVE_DISK_IO;
    if(!same(s->pending,s->check) || !track_save_decode(s->check,identity,&current))return TRACK_SAVE_DISK_VERIFY;
    return TRACK_SAVE_DISK_OK;
}

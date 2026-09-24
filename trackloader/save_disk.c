#include "save_disk.h"
static int same(const unsigned char *a,const unsigned char *b) {
    for(unsigned i=0;i<512;i++)if(a[i]!=b[i])return 0;
    return 1;
}
static int identity(TrackSaveRead read,void *ctx,const TrackSaveMedia *m,unsigned char *b) {
    for(unsigned i=0;i<3;i++) {
        if(!m->expected[i] || m->sector[i]>=1738)return TRACK_SAVE_DISK_IDENTITY;
        if(!read(ctx,m->sector[i],b))return TRACK_SAVE_DISK_IO;
        if(!same(b,m->expected[i]))return TRACK_SAVE_DISK_IDENTITY;
    }
    return TRACK_SAVE_DISK_OK;
}
static int blank(const TrackSaveDiskScratch *s) {
    for(unsigned i=0;i<512;i++)if(s->slots[0][i] || s->slots[1][i])return 0;
    return 1;
}
int track_save_disk_load(TrackSaveRead read,void *ctx,const TrackSaveMedia *m,
    TrackSave *state,TrackSaveDiskScratch *s) {
    TrackSave decoded={0};int status,active,result,read_a,read_b;
    if(!read || !m || !state || !s)return TRACK_SAVE_DISK_STATE;
    status=identity(read,ctx,m,s->check);if(status!=TRACK_SAVE_DISK_OK)return status;
    read_a=read(ctx,TRACK_SAVE_SECTOR_A,s->slots[0]);
    read_b=read(ctx,TRACK_SAVE_SECTOR_B,s->slots[1]);
    /* A torn physical track can be unreadable, not just a bad save CRC.
     * Ignore partial callback output and try the independently placed slot. */
    if(!read_a)for(unsigned i=0;i<512;i++)s->slots[0][i]=0;
    if(!read_b)for(unsigned i=0;i<512;i++)s->slots[1][i]=0;
    active=track_save_select(s->slots[0],s->slots[1],m->identity,&decoded);
    if(active<0 && (!read_a || !read_b))return TRACK_SAVE_DISK_IO;
    if(active==-2 || (active==-1 && !blank(s)))return TRACK_SAVE_DISK_CONFLICT;
    result=active<0 ? TRACK_SAVE_DISK_BLANK:TRACK_SAVE_DISK_OK;
    status=identity(read,ctx,m,s->check);if(status!=TRACK_SAVE_DISK_OK)return status;
    *state=decoded;return result;
}
int track_save_disk_commit(TrackSaveRead read,TrackSaveWrite write,void *ctx,
    const TrackSaveMedia *m,const TrackSave *snapshot,TrackSaveDiskScratch *s) {
    TrackSave current;int status,active,target;uint32_t sector;
    if(!read || !write || !m || !snapshot || !s)return TRACK_SAVE_DISK_STATE;
    if(!track_save_encode(s->pending,m->identity,snapshot))return TRACK_SAVE_DISK_STATE;
    status=identity(read,ctx,m,s->check);if(status!=TRACK_SAVE_DISK_OK)return status;
    if(!read(ctx,TRACK_SAVE_SECTOR_A,s->slots[0]) ||
       !read(ctx,TRACK_SAVE_SECTOR_B,s->slots[1]))return TRACK_SAVE_DISK_IO;
    active=track_save_select(s->slots[0],s->slots[1],m->identity,&current);
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
            return identity(read,ctx,m,s->check);
        if(snapshot->generation!=current.generation+1)return TRACK_SAVE_DISK_CONFLICT;
        target=1-active;
    }
    status=identity(read,ctx,m,s->check);if(status!=TRACK_SAVE_DISK_OK)return status;
    sector=target?TRACK_SAVE_SECTOR_B:TRACK_SAVE_SECTOR_A;
    if(!write(ctx,sector,s->pending))return TRACK_SAVE_DISK_IO;
    if(!read(ctx,sector,s->check))return TRACK_SAVE_DISK_IO;
    if(!same(s->pending,s->check) || !track_save_decode(s->check,m->identity,&current))return TRACK_SAVE_DISK_VERIFY;
    return identity(read,ctx,m,s->check);
}

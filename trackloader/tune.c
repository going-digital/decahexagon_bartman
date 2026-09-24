#include "tune.h"
#include "fib_expand.h"
static unsigned be16(const unsigned char *p) { return ((unsigned)p[0]<<8)|p[1]; }
static unsigned be32(const unsigned char *p) { return (be16(p)<<16)|be16(p+2); }
static unsigned le32(const unsigned char *p) {
    return (unsigned)p[0]|((unsigned)p[1]<<8)|((unsigned)p[2]<<16)|((unsigned)p[3]<<24);
}
int trackloader_tune_check(const unsigned char *p,unsigned bytes,unsigned capacity,TrackTuneInfo *info) {
    TrackTuneInfo result;
    unsigned banks,count,meta,pcm,total=0;
    if(!p || !info || bytes<24 || bytes>capacity) return 0;
    if(p[0]!='T'||p[1]!='U'||p[2]!='N'||p[3]!='1'||be32(p+4)!=12000) return 0;
    banks=be32(p+12);count=be32(p+16);pcm=be32(p+20);
    if(!banks || banks>65535 || !count || count>65535 || !pcm) return 0;
    meta=24+4*(banks+1)+4*count;
    if(meta>bytes || bytes-meta<12 || meta>capacity || pcm>capacity-meta) return 0;
    if(be32(p+24)!=0 || be32(p+24+4*banks)!=pcm) return 0;
    for(unsigned i=0;i<banks;i++) {
        unsigned a=be32(p+24+4*i),b=be32(p+28+4*i);
        if(a>=b || b>pcm) return 0;
    }
    for(unsigned i=0;i<count;i++) {
        const unsigned char *s=p+24+4*(banks+1)+4*i;
        unsigned id=be16(s),n=be16(s+2);
        if(id>=banks || !n || n>32767) return 0;
        unsigned length=be32(p+28+4*id)-be32(p+24+4*id);
        if(n>length || length-n>2 || total+n<total) return 0;
        total+=n;
    }
    if(total!=be32(p+8)) return 0;
    const unsigned char *f=p+meta;
    if(f[0]!='F'||f[1]!='I'||f[2]!='B'||f[3]!='1'||le32(f+4)!=pcm||le32(f+8)!=512) return 0;
    unsigned blocks=(pcm-1)/512,last=pcm-blocks*512;
    if(bytes-meta!=12+blocks*257+1+last/2) return 0;
    result.rate=12000;result.samples=total;result.metadata_bytes=meta;
    result.pcm_bytes=pcm;result.arena_bytes=meta+pcm;*info=result;
    return 1;
}
int trackloader_tune_prepare(unsigned char *p,unsigned bytes,unsigned capacity,PcmSong *song,TrackTuneInfo *info) {
    TrackTuneInfo result;
    if(!song || !info || !trackloader_tune_check(p,bytes,capacity,&result)) return 0;
    unsigned meta=result.metadata_bytes;
    if(trackloader_fib_expand(p+meta,bytes-meta,capacity-meta)!=result.pcm_bytes) return 0;
    PcmSong next={0};
    next.offsets=p+24;next.sequence=p+24+4*(be32(p+12)+1);
    next.bank=p+meta;next.pcm_split=result.pcm_bytes;
    next.seq_count=be32(p+16);
    *song=next;*info=result;
    return 1;
}

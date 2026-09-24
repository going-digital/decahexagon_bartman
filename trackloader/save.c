#include "save.h"
static uint32_t get(const unsigned char *p) {
    return ((uint32_t)p[0]<<24)|((uint32_t)p[1]<<16)|((uint32_t)p[2]<<8)|p[3];
}
static void put(unsigned char *p,uint32_t v) {
    p[0]=(unsigned char)(v>>24);p[1]=(unsigned char)(v>>16);
    p[2]=(unsigned char)(v>>8);p[3]=(unsigned char)v;
}
static uint32_t crc(const unsigned char *p) {
    uint32_t v=0xffffffffUL;
    for(unsigned i=0;i<508;i++) {
        v^=p[i];
        for(unsigned bit=0;bit<8;bit++)v=(v>>1)^((0u-(v&1u))&0xedb88320UL);
    }
    return ~v;
}
int track_save_encode(unsigned char *p,const unsigned char *id,const TrackSave *s) {
    uint32_t mask=0;
    if(!p || !id || !s)return 0;
    for(unsigned i=0;i<6;i++) {
        if(s->records.completed[i]>1)return 0;
        mask|=(uint32_t)s->records.completed[i]<<i;
    }
    for(unsigned i=0;i<512;i++)p[i]=0;
    put(p,0x48585331UL);put(p+4,1);put(p+8,512);put(p+12,s->generation);
    for(unsigned i=0;i<16;i++)p[16+i]=id[i];
    for(unsigned i=0;i<6;i++)put(p+32+4*i,s->records.best[i]);
    put(p+56,mask);put(p+60,s->achievements);put(p+508,crc(p));
    return 1;
}
int track_save_decode(const unsigned char *p,const unsigned char *id,TrackSave *s) {
    TrackSave decoded={0};
    uint32_t mask;
    if(!p || !id || !s || get(p)!=0x48585331UL || get(p+4)!=1 ||
       get(p+8)!=512 || get(p+508)!=crc(p))return 0;
    for(unsigned i=0;i<16;i++)if(p[16+i]!=id[i])return 0;
    for(unsigned i=64;i<508;i++)if(p[i])return 0;
    mask=get(p+56);if(mask&~63UL)return 0;
    for(unsigned i=0;i<6;i++) {
        decoded.records.best[i]=get(p+32+4*i);
        decoded.records.completed[i]=(unsigned char)((mask>>i)&1);
    }
    decoded.achievements=get(p+60);decoded.generation=get(p+12);
    *s=decoded;return 1;
}
int track_save_select(const unsigned char *a,const unsigned char *b,
                      const unsigned char *id,TrackSave *s) {
    TrackSave sa,sb;int va,vb,chosen;uint32_t difference;
    if(!s)return -1;
    va=track_save_decode(a,id,&sa);vb=track_save_decode(b,id,&sb);
    if(!va && !vb)return -1;
    chosen=va?0:1;
    if(va && vb) {
        difference=sb.generation-sa.generation;
        if(difference==0x80000000UL)return -2;
        if(!difference) {
            for(unsigned i=0;i<512;i++)if(a[i]!=b[i])return -2;
        } else if(difference<0x80000000UL)chosen=1;
    }
    *s=chosen?sb:sa;return chosen;
}

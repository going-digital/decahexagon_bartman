#include "ofs.h"
static uint32_t word(const unsigned char *b, unsigned n) {
    b+=n*4;
    return ((uint32_t)b[0]<<24)|((uint32_t)b[1]<<16)|((uint32_t)b[2]<<8)|b[3];
}
static unsigned upper(unsigned c) { return c>='a' && c<='z' ? c-32:c; }
/* Native payloads cannot occupy the reserved bootstrap/save tracks. */
static int block(TrackSectorRead read, void *ctx, uint32_t sector,
                 unsigned char *b, TrackOfsScratch *s) {
    uint32_t sum=0;
    if(sector<2 || sector>=1661 || (s->seen[sector/8]&(1u<<(sector%8))))return 0;
    s->seen[sector/8]|=(unsigned char)(1u<<(sector%8));
    if(!read(ctx,sector,b))return 0;
    for(unsigned i=0;i<128;i++)sum+=word(b,i);
    return sum==0;
}
static int file_metadata(const unsigned char *b,uint32_t sector,unsigned type) {
    return word(b,0)==type && word(b,1)==sector && word(b,2)<=72 &&
        word(b,125)==880 && word(b,127)==0xfffffffdUL;
}
int track_ofs_load(TrackSectorRead read, void *ctx, const char *name,
    unsigned char *dest,uint32_t capacity,uint32_t expected,TrackOfsScratch *s) {
    unsigned len=0,hash;
    uint32_t sector,remaining=expected,sequence=1,next_data=0;
    unsigned char *m,*d;
    if(!read || !name || !dest || !s || !expected || expected>capacity)return 0;
    while(name[len]) {
        unsigned c=(unsigned char)name[len];
        if(len==30 || c<32 || c>126 || c==':' || c=='/')return 0;
        len++;
    }
    if(!len)return 0;
    hash=len;
    for(unsigned i=0;i<len;i++)hash=(hash*13+upper((unsigned char)name[i]))&0x7ff;
    for(unsigned i=0;i<sizeof(s->seen);i++)s->seen[i]=0;
    m=s->metadata;d=s->data;
    if(!block(read,ctx,880,m,s) || word(m,0)!=2 || word(m,3)!=72 ||
       word(m,127)!=1)return 0;
    while(hash>=72)hash-=72; /* bounded by the 11-bit OFS name hash */
    sector=word(m,6+hash);
    for(;;) {
        unsigned same;
        if(!block(read,ctx,sector,m,s) || !file_metadata(m,sector,2) ||
           !m[432] || m[432]>30)return 0;
        same=m[432]==len;
        for(unsigned i=0;same && i<len;i++)
            if(upper(m[433+i])!=upper((unsigned char)name[i]))same=0;
        if(same)break;
        sector=word(m,124);
    }
    if(word(m,81)!=expected)return 0;
    next_data=word(m,4);
    for(;;) {
        uint32_t count=word(m,2),extension=word(m,126);
        if(!count)return 0;
        for(uint32_t i=0;i<count;i++) {
            uint32_t data_sector=word(m,77-i),bytes;
            if(!remaining || data_sector!=next_data ||
               !block(read,ctx,data_sector,d,s) || word(d,0)!=8 ||
               word(d,1)!=sector || word(d,2)!=sequence)return 0;
            bytes=word(d,3);next_data=word(d,4);
            if(!bytes || bytes>488 || bytes>remaining)return 0;
            /* Check termination before copying the final block. */
            if(bytes==remaining && (next_data || extension || i+1!=count))return 0;
            for(uint32_t j=0;j<bytes;j++)*dest++=d[24+j];
            remaining-=bytes;sequence++;
        }
        if(!remaining)return !extension && !next_data;
        sector=extension;
        if(!block(read,ctx,sector,m,s) || !file_metadata(m,sector,16))return 0;
    }
}

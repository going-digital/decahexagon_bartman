/* Offline-decoded, duration-adjusted dictionary. CPU RAM is sufficient: only
 * the four output buffers are read by Paula. FBP1 starts every slice on a word. */
#include "fib_pcm.h"
#include "fib_copy.h"
static unsigned be16(const unsigned char *p) {return ((unsigned)p[0]<<8)|p[1];}
static unsigned be32(const unsigned char *p) {return (be16(p)<<16)|be16(p+2);}
int fib_pcm_init_split(PcmSong *s,const unsigned char *p,unsigned first_bytes,
                       const unsigned char *second,unsigned second_bytes) {
    unsigned bytes=first_bytes+second_bytes;
    if(bytes<first_bytes || first_bytes<32 || (first_bytes&1) ||
       (second_bytes && !second)) return 0;
    if(bytes<32 || p[0]!='F'||p[1]!='B'||p[2]!='P'||(p[3]!='1' && p[3]!='2')) return 0;
    unsigned shared=p[3]=='2';
    unsigned count=be32(p+8),banks=be32(p+12),off=be32(p+16),seq=be32(p+20),data=be32(p+24);
    if(!banks || banks>65535 || !count || count>65535 || off!=32 ||
       seq!=off+4*(banks+1) || data!=seq+4*count || data>first_bytes ||
       be32(p+28)!=bytes || be32(p+off)!=0 || be32(p+off+4*banks)!=bytes-data) return 0;
    unsigned split=first_bytes-data;
    for(unsigned i=0;i<banks;++i) {
        unsigned a=be32(p+off+4*i),b=be32(p+off+4*(i+1));
        if((a|b)&1 || a>=b || b>bytes-data || (a<split && b>split)) return 0;
    }
    unsigned total=0;
    for(unsigned i=0;i<count;++i) {
        unsigned id=be16(p+seq+4*i),n=be16(p+seq+4*i+2);
        if(id>=banks || !n || n>32767) return 0;
        unsigned a=be32(p+off+4*id),b=be32(p+off+4*(id+1));
        const unsigned char *slice=a<split?p+data+a:second+(a-split);
        /* FBP2 permits a shared slice to end up to two bytes beyond the
         * requested duration. These bytes are never copied into the stream. */
        if(shared) {
            if(b-a<n || b-a>n+2) return 0;
        } else if(b-a!=((n+1)&~1u) || ((n&1) && slice[n])) return 0;
        total+=n;
    }
    if(total!=be32(p+4)) return 0;
    s->edges=second;s->pcm_split=split;
    s->sequence=p+seq;s->offsets=p+off;s->bank=p+data;
    s->seq_count=count;s->seq_index=0;s->output_left=0;
    return 1;
}
int fib_song_init(PcmSong *s,const unsigned char *p,unsigned bytes) {
    return fib_pcm_init_split(s,p,bytes,0,0);
}
int fib_song_seek(PcmSong *s,unsigned sample) {
    for(unsigned i=0;i<s->seq_count;++i) {
        const unsigned char *entry=s->sequence+4*i;
        unsigned n=be16(entry+2);
        if(sample<n) {
            unsigned offset=be32(s->offsets+4*be16(entry));
            s->raw=(offset<s->pcm_split?s->bank+offset:s->edges+(offset-s->pcm_split))+sample;
            s->seq_index=i+1;
            s->output_left=n-sample;
            return 1;
        }
        sample-=n;
    }
    return 0;
}
void fib_song_read(PcmSong *s,unsigned char *out,unsigned samples) {
    while(samples) {
        if(!s->output_left) {
            if(s->seq_index==s->seq_count) s->seq_index=0;
            const unsigned char *entry=s->sequence+4*s->seq_index++;
            unsigned offset=be32(s->offsets+4*be16(entry));
            s->raw=offset<s->pcm_split?s->bank+offset:s->edges+(offset-s->pcm_split);
            s->output_left=be16(entry+2);
        }
        unsigned n=samples<s->output_left?samples:s->output_left;
        fib_copy_samples(out,s->raw,n);
        s->raw+=n;out+=n;samples-=n;s->output_left-=n;
    }
}

#include "fib_song.h"
#include "fib_decode.h"
static unsigned be16(const unsigned char *p) {return ((unsigned)p[0]<<8)|p[1];}
static unsigned be32(const unsigned char *p) {return (be16(p)<<16)|be16(p+2);}
static unsigned le32(const unsigned char *p) {return (unsigned)p[0]|((unsigned)p[1]<<8)|((unsigned)p[2]<<16)|((unsigned)p[3]<<24);}
static void refill(FibSong *s) {
    unsigned n=s->left>512?512:s->left;
    fib_decode_block(s->packed,s->cache,n);
    s->packed+=1+n/2;s->left-=n;
    s->cache_count=n;s->cache_pos=0;
}
static int next_sample(FibSong *s) {
    if(s->cache_pos==s->cache_count) refill(s);
    return ((signed char*)s->cache)[s->cache_pos++];
}
/* Signed nearest-even rounding. The validated denominator is below 32768;
 * the quotient magnitude is at most 255, so DIVS cannot overflow. */
static int rounded(int anchor,int numerator,unsigned denominator) {
    int q,r;
#ifdef __m68k__
    int packed=numerator;
    __asm ("divs.w %1,%0" : "+d"(packed) : "d"(denominator) : "cc");
    q=(short)packed;r=(short)((unsigned)packed>>16);
#else
    q=numerator/(int)denominator;r=numerator%(int)denominator;
#endif
    unsigned magnitude=r<0?-r:r;
    if(magnitude*2>denominator || (magnitude*2==denominator && ((anchor+q)&1))) q+=numerator<0?-1:1;
    return anchor+q;
}
static void begin_segment(FibSong *s) {
    if(s->seq_index==s->seq_count) {s->seq_index=0;s->raw=s->edges;}
    const unsigned char *entry=s->sequence+4*s->seq_index++;
    unsigned id=be16(entry);
    s->target_length=s->output_left=be16(entry+2);
    s->raw_mode=id>=65534;
    if(s->raw_mode) return;
    const unsigned char *f=s->bank+be32(s->offsets+4*id);
    s->source_length=s->left=le32(f+4);
    s->packed=f+12;s->cache_pos=s->cache_count=0;
    s->source_index=s->phase=0;
    if(s->source_length!=s->target_length) {s->a=next_sample(s);s->b=next_sample(s);}
}
int fib_song_init(FibSong *s,const unsigned char *p,unsigned bytes) {
    if(bytes<32 || p[0]!='F'||p[1]!='B'||p[2]!='S'||p[3]!='1') return 0;
    unsigned count=be32(p+8),banks=be32(p+12),off=be32(p+16),seq=be32(p+20),data=be32(p+24),edges=be32(p+28);
    if(!banks || banks>=65534 || count<2 || count>65535 || off!=32 || seq!=off+4*(banks+1) || data!=seq+4*count || data>edges || edges>bytes) return 0;
    if(be32(p+off)!=0 || be32(p+off+4*banks)!=edges-data) return 0;
    for(unsigned i=0;i<banks;++i) {
        unsigned a=be32(p+off+4*i),b=be32(p+off+4*(i+1));
        if(a>b || b>edges-data || b-a<12) return 0;
        const unsigned char *f=p+data+a;
        unsigned n=le32(f+4);
        if(f[0]!='F'||f[1]!='I'||f[2]!='B'||f[3]!='1'||le32(f+8)!=512||n<2||n>32767) return 0;
        unsigned full=n/512,tail=n%512;
        if(b-a!=12+257*full+(tail?1+tail/2:0)) return 0;
    }
    unsigned total=0,raw=0;
    for(unsigned i=0;i<count;++i) {
        unsigned id=be16(p+seq+4*i),n=be16(p+seq+4*i+2);
        if(!n || n>32767) return 0;
        if(i==0 || i==count-1) {
            if(id!=(i?65534:65535)) return 0;
            raw+=n;
        } else {
            if(id>=banks || n<2) return 0;
            unsigned source=le32(p+data+be32(p+off+4*id)+4);
            if(source+1<n || n+1<source) return 0;
        }
        total+=n;
    }
    if(total!=be32(p+4) || raw!=bytes-edges) return 0;
    s->blob=p;s->sequence=p+seq;s->offsets=p+off;s->bank=p+data;s->edges=p+edges;
    s->seq_count=count;s->bank_count=banks;s->seq_index=0;s->raw=s->edges;s->output_left=0;
    return 1;
}
#ifdef __clang__
__attribute__((noinline))
#else
__attribute__((noinline,noclone))
#endif
void fib_song_read(FibSong *s,unsigned char *out,unsigned samples) {
    while(samples) {
        if(!s->output_left) begin_segment(s);
        unsigned n=samples<s->output_left?samples:s->output_left;
        if(s->raw_mode) {
            __builtin_memcpy(out,s->raw,n);s->raw+=n;
        } else if(s->source_length==s->target_length) {
            if(s->cache_pos==s->cache_count) refill(s);
            unsigned available=s->cache_count-s->cache_pos;
            if(n>available) n=available;
            __builtin_memcpy(out,s->cache+s->cache_pos,n);s->cache_pos+=n;
        } else {
            unsigned phase=s->phase,index=s->source_index;
            unsigned den=s->target_length-1,step=s->source_length-1;
            int simple=s->output_left>2 && !(s->source_length<s->target_length && s->output_left==s->target_length);
            if(simple) {if(n>s->output_left-2u) n=s->output_left-2u;}
            else if(s->source_length<s->target_length && s->output_left==s->target_length) n=1;
            int direction=s->source_length>s->target_length?1:-1;
            int a=s->a,b=s->b;
            signed char *cursor=(signed char*)s->cache+s->cache_pos;
            signed char *end=(signed char*)s->cache+s->cache_count;
            for(unsigned i=0;i<n;++i) {
                out[i]=(unsigned char)(phase?rounded(a,(short)(b-a)*(short)phase,den):a);
                if(simple) {
                    /* Lengths differ by exactly one: phase walks by one and
                     * source advances once, except at separately handled ends. */
                    phase+=direction;++index;a=b;
                    if(cursor==end) {refill(s);cursor=(signed char*)s->cache;end=cursor+s->cache_count;}
                    b=*cursor++;
                } else if(s->output_left-i>1) {
                    phase+=step;
                    while(phase>=den) {
                        phase-=den;a=b;
                        if(++index<step) {
                            if(cursor==end) {
                                refill(s);cursor=(signed char*)s->cache;end=cursor+s->cache_count;
                            }
                            b=*cursor++;
                        } else b=a;
                    }
                }
            }
            s->cache_pos=cursor-(signed char*)s->cache;
            s->a=a;s->b=b;s->phase=phase;s->source_index=index;
        }
        s->output_left-=n;samples-=n;out+=n;
    }
}

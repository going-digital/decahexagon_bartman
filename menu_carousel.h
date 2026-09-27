#pragma once
#ifdef __m68k__
#define MENU_COLD static __attribute__((noinline,optimize("Os")))
#else
#define MENU_COLD static inline
#endif
/* Three 192-pixel-spaced labels, repeated to allow a contiguous 320px window. */
#define MENU_STRIP_BYTES 188
#define MENU_STRIP_SIZE (MENU_STRIP_BYTES*16)
MENU_COLD void menu_strip_init_for(unsigned char *strip,unsigned levels) {
    static const char *items[]={"START","OPTIONS","CREDITS","HEXAGON","HEXAGONER","HEXAGONEST"};
    for(unsigned i=0;i<MENU_STRIP_SIZE;++i)strip[i]=0;
    for(unsigned n=0;n<(levels?8u:5u);++n) {
        const char *s=(levels & (256u<<(n%6)))?"LOCKED":items[n%3+(levels?3:0)];unsigned len=0;while(s[len])++len;
        int left=160+(int)n*192-(int)len*8;
        for(unsigned c=0;c<len;++c)for(unsigned y=0;y<16;++y) {
            unsigned x=(unsigned)left+c*16;
            if(x+16>MENU_STRIP_BYTES*8)continue;
            unsigned bits=menu_font16[(unsigned)s[c]-'A'][y];
            strip[y*MENU_STRIP_BYTES+x/8]=bits>>8;
            strip[y*MENU_STRIP_BYTES+x/8+1]=bits;
        }
    }
    /* The previous label is centred at -32; keep its visible right edge. */
    for(unsigned y=0;y<16;++y)for(unsigned x=0;x<8;++x)
        strip[y*MENU_STRIP_BYTES+x]=strip[y*MENU_STRIP_BYTES+(levels?144:72)+x];
}
static inline void menu_strip_init(unsigned char *strip) {menu_strip_init_for(strip,0);}
static inline void menu_strip_window(const unsigned char *strip,unsigned char *plane,unsigned position) {
    unsigned byte=position>>3,shift=position&7;
    for(unsigned y=0;y<16;++y)for(unsigned x=0;x<40;++x) {
        const unsigned char *p=strip+y*MENU_STRIP_BYTES+byte+x;
        plane[(92+y)*40+x]=(unsigned char)((p[0]<<shift)|(p[1]>>(8-shift)));
    }
}

#undef MENU_COLD

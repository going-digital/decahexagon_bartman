#include "../player_shape.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

static unsigned pixel(const PlayerShape *s, unsigned x, unsigned y) {
    return !!(s->rows[y][x>>4] & (0x8000u>>(x&15)));
}

static void check(const int16_t xs[3], const int16_t ys[3]) {
    PlayerShape a,b;
    assert(player_shape_build(&a,xs,ys));
    assert(a.width && a.width<=32 && a.height && a.height<=32);
    for (unsigned i=0;i<3;++i) assert(pixel(&a,xs[i]-a.x,ys[i]-a.y));
    for (unsigned y=0;y<a.height;++y) {
        unsigned seen=0, ended=0;
        for (unsigned x=0;x<32;++x) {
            unsigned set=pixel(&a,x,y);
            assert(x<a.width || !set);
            assert(!set || !ended);
            if (set) seen=1;
            else if (seen) ended=1;
        }
        assert(seen); // no empty stripes within the marker
    }
    const unsigned permutations[6][3]={{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}};
    for (unsigned p=0;p<6;++p) {
        int16_t x[3],y[3];
        for (unsigned i=0;i<3;++i) {x[i]=xs[permutations[p][i]]-123; y[i]=ys[permutations[p][i]]+71;}
        assert(player_shape_build(&b,x,y));
        assert(b.x==a.x-123 && b.y==a.y+71);
        assert(b.width==a.width && b.height==a.height);
        assert(!memcmp(a.rows,b.rows,a.height*sizeof(a.rows[0])));
    }
}

int main(void) {
    PlayerShape s;
    int16_t x[3]={0,4,0}, y[3]={0,0,4};
    assert(player_shape_build(&s,x,y));
    for (unsigned row=0;row<5;++row)
        for (unsigned col=0;col<32;++col) assert(pixel(&s,col,row)==(col<=4-row));
    check(x,y);
    x[0]=0; x[1]=31; x[2]=16; y[0]=y[1]=y[2]=0;
    check(x,y); assert(player_shape_build(&s,x,y));
    assert(s.rows[0][0]==0xffff && s.rows[0][1]==0xffff);
    x[0]=x[1]=x[2]=7; y[0]=0; y[1]=31; y[2]=16; check(x,y);
    x[0]=x[1]=x[2]=15; y[0]=y[1]=y[2]=15; check(x,y);
    x[0]=-32768; x[1]=32767; assert(!player_shape_build(&s,x,y));
    assert(!s.width && !s.height);
    uint32_t rng=19;
    for (unsigned n=0;n<10000;++n) {
        for (unsigned i=0;i<3;++i) {
            rng=rng*1664525u+1013904223u; x[i]=(rng>>24)&31;
            rng=rng*1664525u+1013904223u; y[i]=(rng>>24)&31;
        }
        check(x,y);
    }
    puts("Player shape: fixtures and 10,000 triangles, all vertex permutations/translations passed");
}

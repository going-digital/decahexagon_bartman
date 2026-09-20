#include "player_shape.h"

unsigned player_shape_build(PlayerShape *s, const int16_t xs[3], const int16_t ys[3]) {
    int16_t xmin=xs[0], xmax=xs[0], ymin=ys[0], ymax=ys[0];
    for (unsigned i=1;i<3;++i) {
        if (xs[i]<xmin) xmin=xs[i];
        if (xs[i]>xmax) xmax=xs[i];
        if (ys[i]<ymin) ymin=ys[i];
        if (ys[i]>ymax) ymax=ys[i];
    }
    s->x=xmin; s->y=ymin; s->width=s->height=0;
    if ((int32_t)xmax-xmin>=PLAYER_SHAPE_SIZE ||
        (int32_t)ymax-ymin>=PLAYER_SHAPE_SIZE) return 0;
    s->width=xmax-xmin+1; s->height=ymax-ymin+1;
    int16_t left[PLAYER_SHAPE_SIZE], right[PLAYER_SHAPE_SIZE];
    for (unsigned y=0;y<s->height;++y) {left[y]=PLAYER_SHAPE_SIZE; right[y]=-1;}
    /* Bresenham edges give inclusive row bounds without division. The tiny
     * triangle is convex, so filling between those bounds produces a solid
     * marker, including its nose and horizontal edges. */
    for (unsigned i=0;i<3;++i) {
        unsigned j=i==2?0:i+1;
        int16_t x=xs[i]-xmin, y=ys[i]-ymin;
        int16_t x1=xs[j]-xmin, y1=ys[j]-ymin;
        /* Canonical edge direction makes winding irrelevant at tie pixels. */
        if (y>y1 || (y==y1 && x>x1)) {
            int16_t t=x; x=x1; x1=t; t=y; y=y1; y1=t;
        }
        int16_t dx=x1>x?x1-x:x-x1, dy=y1-y;
        int16_t sx=x<x1?1:-1, err=dx-dy;
        for (;;) {
            if (x<left[y]) left[y]=x;
            if (x>right[y]) right[y]=x;
            if (x==x1 && y==y1) break;
            int16_t e2=err*2;
            if (e2>-dy) {err-=dy; x+=sx;}
            if (e2<dx) {err+=dx; ++y;}
        }
    }
    for (unsigned y=0;y<s->height;++y) {
        unsigned l=(unsigned)left[y], r=(unsigned)right[y];
        s->rows[y][0]= l>=16?0:(uint16_t)((0xffffu>>l) &
            (r>=15?0xffffu:(0xffffu<<(15-r))));
        s->rows[y][1]= r<16?0:(uint16_t)((l<=16?0xffffu:(0xffffu>>(l-16))) &
            (0xffffu<<(31-r)));
    }
    return 1;
}

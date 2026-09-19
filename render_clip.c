#include "render_clip.h"
#include "config.h"
static int16_t intersect(int16_t c,int16_t n,int16_t d) {
#ifdef __m68k__
    int32_t r=c;
    __asm__("muls.w %1,%0\n\tdivs.w %2,%0" : "+&d"(r) : "d"(n),"d"(d) : "cc");
    return (int16_t)r;
#else
    return (int16_t)(((int32_t)c*n)/d);
#endif
}
void render_clip_line(int16_t x0,int16_t y0,int16_t x1,int16_t y1,RenderClip *out) {
    int16_t t;
    out->line=out->fix=0;
    if (y0>y1) {t=x0;x0=x1;x1=t;t=y0;y0=y1;y1=t;}
    if (y1<0 || y0>YMAX || y0==y1) return;
    /* Clip Y unconditionally, even when the intersection lies outside X.
     * Then X clipping operates on a segment already confined vertically. */
    if (y0<0) {x0-=intersect(y0,x1-x0,y1-y0);y0=0;}
    if (y1>YMAX) {x1+=intersect(YMAX-y1,x1-x0,y1-y0);y1=YMAX;}
    if (x0>x1) {t=x0;x0=x1;x1=t;t=y0;y0=y1;y1=t;}
    if (x1<0) return;
    if (x0>XMAX) {
        out->fix=1;out->fix_y0=y0;out->fix_y1=y1;return;
    }
    if (x0<0) {y0-=intersect(x0,y1-y0,x1-x0);x0=0;}
    if (x1>XMAX) {
        int16_t ny=(int16_t)(y1+intersect(XMAX-x1,y1-y0,x1-x0));
        out->fix=1;out->fix_y0=y1;out->fix_y1=ny;
        x1=XMAX;y1=ny;
    }
    out->x0=x0;out->y0=y0;out->x1=x1;out->y1=y1;out->line=(y0!=y1);
}

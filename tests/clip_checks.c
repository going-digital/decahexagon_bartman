#include "../render_clip.h"
#define CHECK(e) do { if (!(e)) return 3000+__LINE__; } while (0)
unsigned render_clip_checks(void) {
    RenderClip c;
    /* Old clipper passed (0,0)->(722,-369) to the blitter for this near miss. */
    render_clip_line(722,-369,-1115,568,&c);
    CHECK(!c.line && !c.fix);
    render_clip_line(160,-100,160,300,&c);
    CHECK(c.line && !c.fix && c.x0==160 && c.x1==160 && c.y0==0 && c.y1==199);
    render_clip_line(400,-100,400,300,&c);
    CHECK(!c.line && c.fix && c.fix_y0==0 && c.fix_y1==199);
    render_clip_line(200,10,500,100,&c);
    CHECK(c.line && c.fix && c.x1==319 && c.y1==46 && c.fix_y0==100 && c.fix_y1==46);
    uint32_t random=1;
#ifdef __m68k__
    const unsigned cases=512;
#else
    const unsigned cases=1000000;
#endif
    for (unsigned i=0;i<cases;++i) {
        int16_t p[4];
        for (unsigned j=0;j<4;++j) {
            random=random*1664525u+1013904223u;
            p[j]=(int16_t)((random>>8)%16000)-8000;
        }
        render_clip_line(p[0],p[1],p[2],p[3],&c);
        if (c.line) {
            CHECK(c.x0>=0 && c.x0<=319 && c.x1>=0 && c.x1<=319);
            CHECK(c.y0>=0 && c.y0<=199 && c.y1>=0 && c.y1<=199);
        }
        if (c.fix) CHECK(c.fix_y0>=0 && c.fix_y0<=199 && c.fix_y1>=0 && c.fix_y1<=199);
    }
    return 0;
}

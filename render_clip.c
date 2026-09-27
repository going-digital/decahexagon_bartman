#include "render_clip_impl.h"

void render_clip_line(int16_t x0,int16_t y0,int16_t x1,int16_t y1,RenderClip *out) {
    render_clip_line_inline(x0,y0,x1,y1,out);
}

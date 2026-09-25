#include "render_ending.h"
#include "blitter.h"
#include "config.h"
int render_ending_quad(void *buffer,const PcEndingView *view,
                       const PcEndingVertex vertices[4]) {
    if(!buffer)return -1;
    PcEndingPoint points[PC_END_CLIP_CAPACITY];
    int n=pc_ending_polygon(view,vertices,XMAX,YMAX,points);
    if(n<=0)return n;
    /* Use the same parity/endpoint convention as ordinary playfield polygons.
     * Clipping has already reduced every endpoint to viewport coordinates. */
    PcEndingPoint prev=points[n-1];
    for(int i=0;i<n;i++) {
        blit_clipped_line_onedot((WORD)prev.x,(WORD)prev.y,
                                (WORD)points[i].x,(WORD)points[i].y,0,buffer);
        prev=points[i];
    }
    return n;
}

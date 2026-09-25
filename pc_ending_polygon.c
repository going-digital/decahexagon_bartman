#include "pc_ending_polygon.h"
int pc_ending_polygon(const PcEndingView *view,const PcEndingVertex vertices[4],
                      int16_t xmax,int16_t ymax,
                      PcEndingPoint output[PC_END_CLIP_CAPACITY]) {
    if(!view || !vertices || !output || xmax<=0 || ymax<=0)return -1;
    PcEndingPoint projected[4],clipped[PC_END_CLIP_CAPACITY];
    for(unsigned i=0;i<4;i++)
        if(!pc_ending_project_wide(view,vertices[i].x,vertices[i].y,vertices[i].z,
                                  &projected[i].x,&projected[i].y))return -1;
    /* Every other vertex must lie on the same side of every directed edge.
     * This rejects both concavity and crossed vertex order before clipping.
     * With the projection's billion-unit bound, cross products fit int64. */
    int winding=0;
    for(unsigned i=0;i<4;i++) {
        unsigned j=(i+1)%4;
        long long dx=(long long)projected[j].x-projected[i].x;
        long long dy=(long long)projected[j].y-projected[i].y;
        for(unsigned k=0;k<4;k++) {
            long long cross=dx*((long long)projected[k].y-projected[i].y)-
                            dy*((long long)projected[k].x-projected[i].x);
            int sign=cross>0 ? 1:cross<0 ? -1:0;
            if(sign && winding && sign!=winding)return -1;
            if(sign)winding=sign;
        }
    }
    if(!winding)return 0;
    unsigned n=pc_ending_clip_quad(projected,xmax,ymax,clipped);
    for(unsigned i=0;i<n;i++)output[i]=clipped[i];
    return (int)n;
}

#include "../pc_ending_wall.h"
#include "../pc_ending_affine.h"
#include <assert.h>
#include <stdio.h>
int main(void) {
 PcSpan span={100,200,0};PcEndingVertex p[4];
 PcEndingDirection a={16384,0},b={0,16384};
 assert(pc_ending_wall_quad(&span,10,a,b,p));
 assert(p[0].x==110 && p[0].y==0 && p[1].x==0 && p[1].y==110);
 assert(p[2].x==0 && p[2].y==210 && p[3].x==210 && p[3].y==0);
 PcEndingView v={20,-10,600,160,100,300,300};PcEndingAffine m;
 int16_t xs[4],ys[4];assert(pc_ending_affine_prepare(&m,&v));
 assert(pc_ending_affine_quad(&m,p,xs,ys));
 int old=p[0].x;span.outer=8192;
 assert(!pc_ending_wall_quad(&span,1,a,b,p) && p[0].x==old);
 span.inner=span.outer;assert(!pc_ending_wall_quad(&span,0,a,b,p));
 span=(PcSpan){0,8192,0};a=(PcEndingDirection){-16384,0};
 assert(pc_ending_wall_quad(&span,0,a,b,p) && p[3].x==-8192);
 puts("Ending wall: radial geometry, pulse, bounds and affine handoff pass");
}

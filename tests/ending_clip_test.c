#include "../pc_ending_clip.h"
#include "../pc_ending_projection.h"
#include <assert.h>
#include <stdio.h>
static void rectangle(int x0,int y0,int x1,int y1) {
    PcEndingPoint p[8]={{x0,y0},{x1,y0},{x1,y1},{x0,y1}};
    unsigned n=pc_ending_clip_quad(p,319,199,p);
    int left=x0<0?0:x0,right=x1>319?319:x1;
    int top=y0<0?0:y0,bottom=y1>199?199:y1;
    if(left>=right || top>=bottom){assert(n==0);return;}
    assert(n==4);
    for(unsigned i=0;i<n;i++) {
        assert(p[i].x==left || p[i].x==right);
        assert(p[i].y==top || p[i].y==bottom);
    }
}
int main(void) {
    const int xs[]={-32768,-1,0,1,160,318,319,320,32767};
    const int ys[]={-32768,-1,0,1,100,198,199,200,32767};
    unsigned count=0;
    for(unsigned a=0;a<9;a++)for(unsigned b=a;b<9;b++)
    for(unsigned c=0;c<9;c++)for(unsigned d=c;d<9;d++) {
        rectangle(xs[a],ys[c],xs[b],ys[d]);count++;
    }
    rectangle(-1000000000,-1000000000,1000000000,1000000000);
    PcEndingView view={0,0,0,160,100,600,600};
    PcEndingPoint wide[4],clipped[8];
    const int16_t world[4][2]={{-100,-100},{100,-100},{100,100},{-100,100}};
    for(unsigned i=0;i<4;i++) {
        assert(pc_ending_project_wide(&view,world[i][0],world[i][1],0,&wide[i].x,&wide[i].y));
        assert(wide[i].x>32767 || wide[i].x< -32767);
    }
    assert(pc_ending_clip_quad(wide,319,199,clipped)==4);
    for(unsigned i=0;i<4;i++) {
        assert(clipped[i].x==0 || clipped[i].x==319);
        assert(clipped[i].y==0 || clipped[i].y==199);
    }
    wide[0].x=1000000001;
    assert(!pc_ending_clip_quad(wide,319,199,clipped));
    PcEndingPoint p[4]={{160,-100},{460,100},{160,300},{-140,100}},q[8];
    unsigned n=pc_ending_clip_quad(p,319,199,q);assert(n==8);
    for(unsigned i=0;i<n;i++)assert(q[i].x>=0 && q[i].x<=319 && q[i].y>=0 && q[i].y<=199);
    PcEndingPoint reversed[4];for(unsigned i=0;i<4;i++)reversed[i]=p[3-i];
    assert(pc_ending_clip_quad(reversed,319,199,q)==8);
    assert(!pc_ending_clip_quad(p,0,199,q));
    printf("Ending clipping: %u rectangle cases, enclosing/offscreen/degenerate and eight-vertex clipping pass\n",count);
}

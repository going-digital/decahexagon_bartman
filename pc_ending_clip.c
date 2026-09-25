#include "pc_ending_clip.h"
static int32_t coordinate(PcEndingPoint p,unsigned edge) {
    return edge<2 ? p.x:p.y;
}
static unsigned inside(PcEndingPoint p,unsigned edge,int32_t bound) {
    int32_t c=coordinate(p,edge);
    return (edge&1) ? c<=bound:c>=bound;
}
static PcEndingPoint crossing(PcEndingPoint a,PcEndingPoint b,
                               unsigned edge,int32_t bound) {
    int32_t start=coordinate(a,edge),end=coordinate(b,edge);
    /* Differences can exceed signed16; products can exceed signed32. The
     * crossing lies between endpoints, so the final value fits signed32. */
    if(edge<2) {
        a.y=(int32_t)(a.y+((long long)b.y-a.y)*(bound-start)/(end-start));
        a.x=(int16_t)bound;
    } else {
        a.x=(int32_t)(a.x+((long long)b.x-a.x)*(bound-start)/(end-start));
        a.y=(int16_t)bound;
    }
    return a;
}
static unsigned append(PcEndingPoint *p,unsigned n,PcEndingPoint value) {
    if(n && p[n-1].x==value.x && p[n-1].y==value.y)return n;
    p[n]=value;return n+1;
}
unsigned pc_ending_clip_quad(const PcEndingPoint quad[4],int16_t xmax,int16_t ymax,
                            PcEndingPoint output[PC_END_CLIP_CAPACITY]) {
    if(!quad || !output || xmax<=0 || ymax<=0)return 0;
    /* Convex quad gains at most one vertex per clipping plane. One extra
     * scratch slot permits a repeated closing vertex before deduplication. */
    PcEndingPoint a[9],b[9];unsigned n=4;
    for(unsigned i=0;i<4;i++) {
        if(quad[i].x< -1000000000 || quad[i].x>1000000000 ||
           quad[i].y< -1000000000 || quad[i].y>1000000000)return 0;
        a[i]=quad[i];
    }
    for(unsigned edge=0;edge<4 && n;edge++) {
        int32_t bound=edge==1 ? xmax:edge==3 ? ymax:0;
        unsigned count=0;PcEndingPoint prev=a[n-1];
        unsigned was=inside(prev,edge,bound);
        for(unsigned i=0;i<n;i++) {
            PcEndingPoint next=a[i];unsigned now=inside(next,edge,bound);
            if(now!=was)count=append(b,count,crossing(prev,next,edge,bound));
            if(now)count=append(b,count,next);
            prev=next;was=now;
        }
        if(count>1 && b[0].x==b[count-1].x && b[0].y==b[count-1].y)--count;
        n=count;for(unsigned i=0;i<n;i++)a[i]=b[i];
    }
    if(n<3)return 0;
    long long area=0;
    for(unsigned i=0;i<n;i++) {
        unsigned j=(i+1)%n;
        area+=(long long)a[i].x*a[j].y-(long long)a[j].x*a[i].y;
    }
    if(!area)return 0;
    for(unsigned i=0;i<n;i++)output[i]=a[i];
    return n;
}

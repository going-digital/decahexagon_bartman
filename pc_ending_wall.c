#include "pc_ending_wall.h"
int pc_ending_wall_quad(const PcSpan *span,int16_t pulse,
                        PcEndingDirection first,PcEndingDirection next,
                        PcEndingVertex output[4]) {
    if(!span || !output)return 0;
    int32_t inner=(int32_t)span->inner+pulse,outer=(int32_t)span->outer+pulse;
    if(inner<0 || outer<=inner || outer>8192)return 0;
    if(first.x< -16384 || first.x>16384 || first.y< -16384 || first.y>16384 ||
       next.x< -16384 || next.x>16384 || next.y< -16384 || next.y>16384)return 0;
    PcEndingVertex p[4];
    for(unsigned i=0;i<4;i++) {
        PcEndingDirection d=(i==0 || i==3) ? first:next;
        int16_t r=(int16_t)(i<2 ? inner:outer);
        p[i]=(PcEndingVertex){((int32_t)d.x*r)/16384,((int32_t)d.y*r)/16384,0};
    }
    for(unsigned i=0;i<4;i++)output[i]=p[i];
    return 1;
}

#include "pc_ending_scene.h"
int pc_ending_scene_walls(PcEndingScene *s,const PcWorld *w,
                         const PcEndingView *view,unsigned sides,int16_t pulse,
                         const PcEndingDirection *dirs,PcEndingEdges emit,
                         void *context,unsigned *rejected) {
    if(rejected)*rejected=0;
    if(!s || !w || !view || !dirs || !emit || sides<3 || sides>6 ||
       w->count>PC_WALL_CAPACITY)return -1;
    const PcEndingAffine *matrix=pc_ending_affine_cached(&s->camera,view);
    if(!matrix)return -1;
    unsigned n=pc_project_spans(w,(uint8_t)sides,s->spans),drawn=0;
    for(unsigned i=0;i<n;i++) {
        PcSpan *span=&s->spans[i];unsigned next=span->slot+1;
        if(next==sides)next=0;
        PcEndingVertex quad[4];int16_t xs[4],ys[4];
        if(!pc_ending_wall_quad(span,pulse,dirs[span->slot],dirs[next],quad) ||
           !pc_ending_affine_quad(matrix,quad,xs,ys)) {
            if(rejected)++*rejected;
            continue;
        }
        emit(context,xs,ys);++drawn;
    }
    return (int)drawn;
}
unsigned pc_ending_scene_hub(const PcEndingAffine *matrix,unsigned sides,int16_t radius,
                             const PcEndingDirection *dirs,int16_t xs[6],int16_t ys[6]) {
    if(!matrix || !dirs || !xs || !ys || sides<3 || sides>6 || radius<=0 || radius>8192)return 0;
    int16_t tx[6],ty[6];
    for(unsigned i=0;i<sides;i++) {
        if(dirs[i].x< -16384 || dirs[i].x>16384 || dirs[i].y< -16384 || dirs[i].y>16384)return 0;
        int16_t x=((int32_t)dirs[i].x*radius)/16384;
        int16_t y=((int32_t)dirs[i].y*radius)/16384;
        int32_t sx,sy;
        if(!pc_ending_affine_point(matrix,x,y,&sx,&sy) || sx< -8191 || sx>8191 || sy< -8191 || sy>8191)return 0;
        tx[i]=(int16_t)sx;ty[i]=(int16_t)sy;
    }
    for(unsigned i=0;i<sides;i++){xs[i]=tx[i];ys[i]=ty[i];}
    return sides;
}
unsigned pc_ending_scene_player(const PcEndingAffine *matrix,
                                const PcEndingDirection dirs[3],
                                int16_t tip_radius,int16_t base_radius,PlayerShape *shape) {
    if(!shape)return 0;
    shape->width=shape->height=0;
    if(!matrix || !dirs || base_radius<0 || tip_radius<base_radius || tip_radius>8192)return 0;
    int16_t xs[3],ys[3];
    for(unsigned i=0;i<3;i++) {
        if(dirs[i].x< -16384 || dirs[i].x>16384 || dirs[i].y< -16384 || dirs[i].y>16384)return 0;
        int16_t radius=i ? base_radius:tip_radius;
        int16_t x=((int32_t)dirs[i].x*radius)/16384,y=((int32_t)dirs[i].y*radius)/16384;
        int32_t sx,sy;
        if(!pc_ending_affine_point(matrix,x,y,&sx,&sy) || sx< -8191 || sx>8191 || sy< -8191 || sy>8191)return 0;
        xs[i]=(int16_t)sx;ys[i]=(int16_t)sy;
    }
    return player_shape_build(shape,xs,ys);
}

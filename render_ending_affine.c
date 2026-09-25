#include "render_ending.h"
#include "blitter.h"
int render_ending_affine_quad(void *buffer,const PcEndingAffine *matrix,
                              const PcEndingVertex vertices[4]) {
    int16_t xs[4],ys[4];
    if(!buffer || !pc_ending_affine_quad(matrix,vertices,xs,ys))return 0;
    /* Retain offscreen edges: the established clipper emits right-boundary
     * parity fixes needed when the whole viewport is inside a polygon. */
    for(unsigned i=0;i<4;i++) {
        unsigned j=(i+1)%4;
        blit_clipped_line_onedot(xs[i],ys[i],xs[j],ys[j],0,buffer);
    }
    return 1;
}
int render_ending_affine_wall(void *buffer,const PcEndingAffine *matrix,
                              const PcSpan *span,int16_t pulse,
                              PcEndingDirection first,PcEndingDirection next) {
    PcEndingVertex quad[4];
    if(!pc_ending_wall_quad(span,pulse,first,next,quad))return 0;
    return render_ending_affine_quad(buffer,matrix,quad);
}
static void scene_edges(void *buffer,const int16_t xs[4],const int16_t ys[4]) {
    for(unsigned i=0;i<4;i++) {
        unsigned j=(i+1)%4;
        blit_clipped_line_onedot(xs[i],ys[i],xs[j],ys[j],0,buffer);
    }
}
int render_ending_affine_walls(void *buffer,PcEndingScene *scene,const PcWorld *world,
                               const PcEndingView *view,unsigned sides,int16_t pulse,
                               const PcEndingDirection *directions,unsigned *rejected) {
    if(!buffer)return -1;
    return pc_ending_scene_walls(scene,world,view,sides,pulse,directions,scene_edges,buffer,rejected);
}
int render_ending_affine_hub(void *buffer,const PcEndingAffine *matrix,unsigned sides,
                             int16_t radius,const PcEndingDirection *directions) {
    int16_t xs[6],ys[6];
    if(!buffer || !pc_ending_scene_hub(matrix,sides,radius,directions,xs,ys))return 0;
    unsigned prev=sides-1;
    for(unsigned i=0;i<sides;i++) {
        blit_clipped_line_onedot(xs[prev],ys[prev],xs[i],ys[i],0,buffer);
        prev=i;
    }
    return 1;
}
int render_ending_affine_player(void *buffer,const PcEndingAffine *matrix,
                                const PcEndingDirection directions[3],
                                int16_t tip_radius,int16_t base_radius) {
    PlayerShape shape;
    if(!buffer || !pc_ending_scene_player(matrix,directions,tip_radius,base_radius,&shape))return 0;
    blit_wait();
    for(unsigned y=0;y<shape.height;y++) {
        int sy=shape.y+(int)y;
        if(sy<0 || sy>YMAX)continue;
        for(unsigned x=0;x<shape.width;x++) {
            int sx=shape.x+(int)x;
            if(sx>=0 && sx<=XMAX && (shape.rows[y][x>>4] & (0x8000u>>(x&15))))
                ((UBYTE*)buffer)[sy*SCREEN_WIDTH_BYTES+(sx>>3)]|=0x80u>>(sx&7);
        }
    }
    return 1;
}
int render_ending_flat_frame(void *buffer,PcEndingScene *scene,const PcWorld *world,
                             const PcEndingView *view,unsigned sides,int16_t pulse,
                             const PcEndingDirection *directions,int16_t hub_radius,
                             const PcEndingDirection player_directions[3],
                             int16_t tip_radius,int16_t base_radius,EndingFrameReport *report) {
    if(!buffer || !scene || !world || !view || !directions || !player_directions ||
       !report || sides<3 || sides>6 || world->count>PC_WALL_CAPACITY)return 0;
    const PcEndingAffine *matrix=pc_ending_affine_cached(&scene->camera,view);
    if(!matrix)return 0;
    /* This buffer must not be displayed or used by another DMA operation.
     * blit_line_mode waits for the asynchronous clear before arming lines. */
    blit_cls(buffer);
    blit_fill_reset();
    blit_line_mode();
    EndingFrameReport result={0};
    int walls=render_ending_affine_walls(buffer,scene,world,view,sides,pulse,
                                       directions,&result.rejected_walls);
    if(walls<0){blit_wait();return 0;}
    result.walls=(unsigned)walls;
    result.hub=render_ending_affine_hub(buffer,matrix,sides,hub_radius,directions);
    blit_fill(buffer,buffer);
    result.player=render_ending_affine_player(buffer,matrix,player_directions,tip_radius,base_radius);
    /* Player preparation may reject before waiting; always complete DMA. */
    blit_wait();
    *report=result;return 1;
}

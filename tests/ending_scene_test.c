#include "../pc_ending_scene.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
static unsigned calls;
static void edges(void *context,const int16_t xs[4],const int16_t ys[4]) {
    assert(context==&calls);++calls;
    for(unsigned i=0;i<4;i++)assert(xs[i]>=-8191 && xs[i]<=8191 && ys[i]>=-8191 && ys[i]<=8191);
}
int main(void) {
    static PcEndingScene scene;
    PcWorld world={0};world.count=3;
    world.walls[0]=(PcWall){100,100,0,1};
    world.walls[1]=(PcWall){150,100,0,1};
    world.walls[2]=(PcWall){100,100,1,1};
    PcWorld before=world;
    PcEndingDirection dirs[4]={{16384,0},{0,16384},{-16384,0},{0,-16384}};
    PcEndingView view={20,-10,600,160,100,300,300};unsigned rejected;
    assert(pc_ending_scene_walls(&scene,&world,&view,4,0,dirs,edges,&calls,&rejected)==2);
    assert(calls==2 && rejected==0 && !memcmp(&world,&before,sizeof(world)));
    assert(scene.camera.valid);
    calls=0;view.depth=0;
    assert(pc_ending_scene_walls(&scene,&world,&view,4,0,dirs,edges,&calls,&rejected)==-1 && !calls);
    view.depth=600;
    assert(pc_ending_scene_walls(&scene,&world,&view,4,8192,dirs,edges,&calls,&rejected)==0);
    assert(!calls && rejected==2);
    int16_t xs[6]={0},ys[6]={0};
    PcEndingAffine identity={4096,0,4096,160,100};
    assert(pc_ending_scene_hub(&identity,4,40,dirs,xs,ys)==4);
    assert(xs[0]==200 && ys[0]==100 && xs[1]==160 && ys[1]==140);
    assert(xs[2]==120 && ys[2]==100 && xs[3]==160 && ys[3]==60);
    int16_t before_x=xs[0];dirs[3].x=16385;
    assert(!pc_ending_scene_hub(&identity,4,40,dirs,xs,ys) && xs[0]==before_x);
    assert(!pc_ending_scene_hub(&identity,7,40,dirs,xs,ys));
    assert(!pc_ending_scene_hub(&identity,4,0,dirs,xs,ys));
    PcEndingDirection player_dirs[3]={{16384,0},{16200,-2400},{16200,2400}};
    PlayerShape shape;
    assert(pc_ending_scene_player(&identity,player_dirs,60,54,&shape));
    assert(shape.width>0 && shape.height>0 && shape.width<=32 && shape.height<=32);
    player_dirs[2].x=16385;
    assert(!pc_ending_scene_player(&identity,player_dirs,60,54,&shape));
    assert(!shape.width && !shape.height);
    puts("Ending scene: merged walls, immutable world, invalid camera and rejected spans pass");
}

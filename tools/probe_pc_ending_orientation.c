/* Reuse the verified private Mach-O loader; intercept geometry before drawing. */
#define main projection_probe_main
#include "probe_pc_ending_projection.c"
#undef main
static void capture_point(void *game,int index,double x,double y,double z,int colour) {
    (void)game;(void)z;(void)colour;
    printf("%d %.12g %.12g\n",index,x,y);
    if(index==5)exit(0);
}
int main(int argc,char **argv) {
    assert(argc==3);load(argv[1]);install_hooks();
    patch(0x100009c80+slide,capture_point);
    unsigned char *game=calloc(1,0x10000),*graphics=calloc(1,0x3000),help[96]={0};
    double sine[360],cosine[360];
    for(int i=0;i<360;++i){sine[i]=sin(i*acos(-1.0)/180);cosine[i]=cos(i*acos(-1.0)/180);}
    double *a=sine,*b=cosine;memcpy(help,&a,8);memcpy(help+0x30,&b,8);
    wr32(game,0x18c,6);wr32(game,0x184,100);wrf(game,0x194,atoi(argv[2]));
    ((void(*)(void*,void*,void*))(0x10004a9b0+slide))(graphics,game,help);
    return 1;
}

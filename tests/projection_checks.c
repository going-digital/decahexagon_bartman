#include "../pc_projection.h"

/* Exercise the public projection, so target runs check the actual DIVS.W
 * path and its fallback against independent signed C division. */
static PcWorld projection_world;
static PcSpan projection_spans[PC_WALL_CAPACITY];
static unsigned check_value(int32_t distance,int32_t width) {
    projection_world.count=1;
    projection_world.walls[0]=(PcWall){distance,width,0,1};
    if (pc_project_spans(&projection_world,6,projection_spans)!=1) return 1;
    int16_t inner=(int16_t)(40+distance/5);
    int16_t outer=(int16_t)(inner+width/5);
    return projection_spans[0].inner!=inner || projection_spans[0].outer!=outer;
}
unsigned pc_projection_checks(void) {
    static const int32_t extremes[]={
        (-2147483647-1),-2147483647,-163845,-163841,-163840,-163839,
        -163836,-163835,-6,-5,-4,-1,0,1,4,5,6,
        163835,163839,163840,163841,163844,163845,2147483647
    };
    for (unsigned i=0;i<sizeof(extremes)/sizeof(extremes[0]);++i) {
        if(check_value(extremes[i],25)) return 4001;
        if(extremes[i]>=5 && check_value(-73,extremes[i])) return 4002;
    }
    /* Every remainder/sign around both hardware/fallback boundaries. */
    for (int32_t d=-40;d<=40;++d) {
        if(check_value(-163840+d,163840+d)) return 4003;
        if(check_value(163840+d,163840-d)) return 4004;
    }
#ifdef __m68k__
    const int32_t limit=20000;
#else
    const int32_t limit=163850;
#endif
    for (int32_t d=-limit;d<=limit;++d)
        if(check_value(d,5+(d+limit))) return 4005;
    return 0;
}

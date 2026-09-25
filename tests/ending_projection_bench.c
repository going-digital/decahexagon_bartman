#include "../pc_ending_projection.h"
#ifdef AFFINE_BENCH
#include "../pc_ending_affine.h"
#endif
/* Deliberately vary coordinates so whole-call constant folding is impossible.
 * Measures projection alone, including loop/call overhead; no custom chips. */
unsigned ending_projection_bench(unsigned count) {
    PcEndingView view={20,-10,600,160,100,300,300};
    unsigned ok=0;
#ifdef AFFINE_BENCH
    PcEndingAffine matrix;
#ifdef FLAT_BENCH
    PcEndingAffineCache cache={0};
    const PcEndingAffine *prepared=pc_ending_affine_cached(&cache,&view);
    if(!prepared)return 0;matrix=*prepared;
#else
    if(!pc_ending_affine_prepare(&matrix,&view))return 0;
#endif
#endif
#ifdef INCREMENTAL_BENCH
    int16_t px=-600,py=-600;
#endif
    for(unsigned i=0;i<count;i++) {
#ifndef INCREMENTAL_BENCH
        int16_t px=(int16_t)((i*73)%1200-600),py=(int16_t)((i*41)%1200-600);
#endif
        int32_t x,y;
#ifdef AFFINE_BENCH
        ok+=pc_ending_affine_point(&matrix,px,py,&x,&y);
#else
        ok+=pc_ending_project_wide(&view,px,py,0,&x,&y);
#endif
#ifdef INCREMENTAL_BENCH
        px+=73;if(px>=600)px-=1200;
        py+=41;if(py>=600)py-=1200;
#endif
    }
    return ok;
}

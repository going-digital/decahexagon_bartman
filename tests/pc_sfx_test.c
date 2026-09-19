#include <assert.h>
#include "../pc_sfx.h"
int main(void) {
    PcSfx s={0};pc_sfx_init(&s);
    for(unsigned i=1;i<46;++i) assert(!pc_sfx_startup(&s));
    assert(pc_sfx_startup(&s)==SFX_BIT(SFX_SUPERHEXAGON));
    assert(!pc_sfx_startup(&s));
    assert(pc_sfx_begin(&s,0,0,0)==(SFX_BIT(SFX_BEGIN)|SFX_BIT(SFX_START)));
    for(unsigned t=1;t<=7201;++t) {
        unsigned expected=0;
        switch(t) {
        case 601:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_LINE);break;
        case 1201:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_TRIANGLE);break;
        case 1801:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_SQUARE);break;
        case 2701:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_PENTAGON);break;
        case 3601:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_HEXAGON);break;
        case 7201:expected=SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_AWESOME);break;
        }
        assert(pc_sfx_live(&s,t)==expected);
    }
    assert(!pc_sfx_death(&s,10,40,40)); /* completion suppresses game-over */
    assert(!pc_sfx_death(&s,67,180,200));
    assert(pc_sfx_death(&s,68,200,220)==SFX_BIT(SFX_WONDERFUL));
    assert(!pc_sfx_death(&s,69,220,240));
    pc_sfx_begin(&s,17,1,0);
    for(unsigned t=1;t<=20;++t)
        assert(pc_sfx_live(&s,t)==(t==17?(SFX_BIT(SFX_EXCELLENT)|SFX_BIT(SFX_START)):0));
    for(unsigned t=2;t<=100;++t)
        assert(pc_sfx_death(&s,t,40,40)==(t==10?SFX_BIT(SFX_GAMEOVER):0));
    pc_sfx_begin(&s,601,0,0);
    for(unsigned t=1;t<=600;++t) assert(!pc_sfx_live(&s,t));
    assert(pc_sfx_live(&s,601)==(SFX_BIT(SFX_EXCELLENT)|SFX_BIT(SFX_START)|SFX_BIT(SFX_RANKUP)|SFX_BIT(SFX_LINE)));
    /* All selected profiles share displayed-score rank thresholds. */
    for(unsigned profile=0;profile<6;++profile) {
        pc_sfx_begin(&s,0,0,profile);
        for(unsigned t=1;t<=3601;++t) pc_sfx_live(&s,t);
        assert(s.completion==(profile==5?3:profile==2?2:1));
        assert(pc_sfx_death(&s,68,200,220)==(profile==5?0:SFX_BIT(SFX_WONDERFUL)));
    }
    return 0;
}

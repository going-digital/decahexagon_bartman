#include "pc_sfx.h"
void pc_sfx_init(PcSfx *s) { s->startup=46; }
uint16_t pc_sfx_startup(PcSfx *s) {
    return s->startup && !--s->startup ? SFX_BIT(SFX_SUPERHEXAGON):0;
}
uint16_t pc_sfx_begin(PcSfx *s,uint32_t best,uint8_t completed,uint8_t profile) {
    s->best=best;s->completed=completed;s->profile=profile;
    s->rank=s->record=s->completion=0;
    return SFX_BIT(SFX_BEGIN)|SFX_BIT(SFX_START);
}
uint16_t pc_sfx_live(PcSfx *s,uint32_t elapsed) {
    static const uint32_t limits[]={600,1200,1800,2700,3600,7200};
    static const uint8_t words[]={SFX_LINE,SFX_TRIANGLE,SFX_SQUARE,SFX_PENTAGON,SFX_HEXAGON,SFX_AWESOME};
    uint16_t result=0;
    /* PC announces reaching the old best once, not each improvement. No
     * announcement on the first run when the stored best starts at zero. */
    if(s->best && !s->record && elapsed>=s->best) {
        s->record=1;result=SFX_BIT(SFX_EXCELLENT)|SFX_BIT(SFX_START);
    }
    if(s->rank<6 && elapsed>limits[s->rank]) {
        result|=SFX_BIT(SFX_RANKUP)|SFX_BIT(words[s->rank++]);
        if(s->rank==5 && !s->completed) {
            s->completed=1;s->completion=s->profile==5?3:s->profile==2?2:1;
        }
    }
    return result;
}
uint16_t pc_sfx_death(const PcSfx *s,unsigned timer,unsigned old_extent,unsigned extent) {
    uint16_t result=timer==10 && !s->completion ? SFX_BIT(SFX_GAMEOVER):0;
    if(old_extent<=200 && extent>200 && s->completion && s->completion!=3)
        result|=SFX_BIT(SFX_WONDERFUL);
    return result;
}

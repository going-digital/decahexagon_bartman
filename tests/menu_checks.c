#include "../pc_menu.h"
#define CHECK(c) do { if (!(c)) return __LINE__+700; } while (0)
unsigned pc_menu_checks(void) {
    PcRecords r={0};
    for (uint8_t p=0;p<6;++p) {
        PcMenu m;pc_menu_reset(&m,p);
        CHECK(pc_menu_profile(&m)==p);
        CHECK(pc_profile_unlocked(&r,p)==(p<3));
        /* Both held follows the PC's left priority. One full revolution. */
        for (unsigned t=0;t<60;++t) pc_menu_tick(&m,3);
        CHECK(pc_menu_profile(&m)==p && m.angle==m.wedge*60+30 && !m.motion);
    }
    for (uint8_t p=0;p<6;++p) {
        CHECK(pc_record_tick(&r,p,3600));
        CHECK(!r.completed[p]);
        CHECK(!pc_record_tick(&r,p,3600));
        CHECK(pc_record_tick(&r,p,3601));
        CHECK(r.completed[p]);
        CHECK(!pc_record_tick(&r,p,10));
        CHECK(r.best[p]==3601);
        if (p<3) CHECK(pc_profile_unlocked(&r,p+3));
        for (uint8_t q=p+1;q<6;++q) CHECK(!r.best[q] && !r.completed[q]);
    }
    /* A long continuation belongs to the selected profile, not active stage. */
    CHECK(pc_record_tick(&r,0,18001));
    CHECK(r.best[0]==18001 && r.best[1]==3601 && r.best[3]==3601);
    return 0;
}

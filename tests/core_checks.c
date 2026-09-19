#include "../pc_core.h"
#include "core_checks.h"
#ifdef __m68k__
#include "../support/gcc8_c_support.h"
#endif
#define CHECK(expr) do { if (!(expr)) return __LINE__; } while (0)

unsigned pc_wave_checks(void);
unsigned pc_schedule_checks(void);
unsigned render_clip_checks(void);

unsigned pc_core_checks(void) {
#ifdef __m68k__
    volatile uint16_t radius=700,zoom=128;
    CHECK(muluw(radius,zoom)==89600u);
    CHECK((muluw(radius,zoom)>>8)==350);
    volatile int16_t negative=-700;
    CHECK(mulsw(negative,(int16_t)zoom)==-89600);
#endif
    PcClock pal = {0}, ntsc = {0};
    uint32_t pal_ticks = 0, ntsc_ticks = 0;
    for (unsigned i = 0; i < 500; ++i) pal_ticks += pc_clock_advance(&pal, 1, 50);
    for (unsigned i = 0; i < 600; ++i) ntsc_ticks += pc_clock_advance(&ntsc, 1, 60);
    CHECK(pal_ticks == 600 && ntsc_ticks == 600 && pal.remainder == 0);
    CHECK(pc_clock_advance(&pal, 5, 50) == 6); /* Missed presentations retain time. */
    CHECK(pc_clock_advance(&ntsc, 65535, 60) == 65535u);
    CHECK(pc_turn(30, PC_INPUT_POSITIVE, 7) == 37);
    CHECK(pc_turn(30, PC_INPUT_POSITIVE, 9) == 39);
    CHECK(pc_turn(358, 3, 7) == 5); /* Source input priority, no cancellation. */
    CHECK(pc_turn(2, PC_INPUT_NEGATIVE, 7) == 355);
    int16_t angle = 30;
    for (unsigned i = 0; i < 360; ++i) angle = pc_turn(angle, 1, 7);
    CHECK(angle == 30);
    CHECK(pc_render_angle(90) == 16384u && pc_render_angle(180) == 32768u);

    /* Original x86-64 routine traces at speed22: verified 2026-09-19. */
    static const int16_t distances[] = {151,150,145,123,122,100};
    static const int16_t widths[] = {199,200,400};
    for (unsigned d = 0; d < 6; ++d) for (unsigned w = 0; w < 3; ++w) {
        PcWall wall = {distances[d],widths[w],0,1};
        PcPlayer p = {30,90,0,0};
        pc_collide(&p,&wall,1,6,22);
        CHECK(p.hit == (d >= 1 && d <= 3));
        CHECK(p.angle == ((d >= 4 && w > 0) ? 90 : 30));
        CHECK(p.previous_angle == p.angle);
    }
    /* Ordered restoration rechecks the slot for later records. */
    PcWall walls[] = {{122,200,0,1},{150,200,1,1}};
    PcPlayer p = {30,90,0,0};
    pc_collide(&p,walls,2,6,22);
    CHECK(p.angle == 90 && p.hit && p.blocked);
    PcWall wall = {22,200,0,1};
    pc_move_wall(&wall,22);
    CHECK(wall.distance == 0 && wall.width == 178 && wall.active);
    wall.width = 22;
    pc_move_wall(&wall,22);
    CHECK(!wall.active && wall.width == 0);
    unsigned failure=pc_wave_checks();
    if (!failure) failure=pc_schedule_checks();
    return failure ? failure:render_clip_checks();
}

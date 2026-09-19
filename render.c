#include "render.h"
#include "config.h"
#include "blitter.h"
#include "trig.h"
#include "game.h"
#include "pc_projection.h"

#define CX (SCREEN_WIDTH / 2)
#define CY (SCREEN_HEIGHT / 2)

#define PLAYER_TIP_LEN   6      // how far the nose pokes past PLAYER_RADIUS
#define PLAYER_HALF_ANG  1500   // half the triangle's angular width

static WORD  ox, oy;  // camera shake offset applied this frame
static UWORD zoom;    // Q8 camera zoom applied this frame (ZOOM_ONE = 1.0)

// radius * zoom, as a single 16x16->32 mulu.w (not the __mulsi3 that
// (LONG)r * zoom compiled to - that was ~1.5ms/frame across all the corners).
static WORD zscale(WORD r) {
    return (WORD)(muluw((UWORD)r, zoom) >> 8);
}

// (worldAngle, pre-scaled radius) -> screen pixel, with shake applied.
static void pt(UWORD ang, WORD rr, WORD* sx, WORD* sy) {
    WORD x, y;
    polar_to_cartesian(ang, (UWORD)rr, &x, &y);
    *sx = CX + ox + x;
    *sy = CY + oy + y;
}

// Closed polygon -> fill-seed edges.
static void poly(const WORD* xs, const WORD* ys, WORD n, void* buf) {
    WORD px = xs[n - 1], py = ys[n - 1];
    for (WORD i = 0; i < n; i++) {
        blit_clipped_line_onedot(px, py, xs[i], ys[i], 0, buf);
        px = xs[i];
        py = ys[i];
    }
}

static void draw_hub(void* buf) {
    WORD xs[MAX_NUM_SIDES], ys[MAX_NUM_SIDES]; // capacity; gamestate.num_sides (runtime) says how many are used
    WORD rr = zscale(HUB_RADIUS);
    UWORD a = gamestate.field_angle;
    UBYTE n = gamestate.num_sides;
    for (WORD i = 0; i < n; i++) {
        pt(a, rr, &xs[i], &ys[i]);
        a -= gamestate.segment_angle;
    }
    poly(xs, ys, n, buf);
}

static void draw_player(void* buf) {
    UWORD a = gamestate.field_angle + gamestate.player_angle;
    WORD rt = zscale(PLAYER_RADIUS + PLAYER_TIP_LEN);
    WORD rb = zscale(PLAYER_RADIUS);
    WORD xs[3], ys[3];
    pt(a, rt, &xs[0], &ys[0]);
    pt(a - PLAYER_HALF_ANG, rb, &xs[1], &ys[1]);
    pt(a + PLAYER_HALF_ANG, rb, &xs[2], &ys[2]);
    poly(xs, ys, 3, buf);
}

// Render-only unions keep overlapping same-slot walls solid under XOR fill.
static PcSpan spans[PC_WALL_CAPACITY];
static UWORD n_active;
static UWORD slot_prev(UBYTE s) { return s ? s-1 : gamestate.num_sides-1; }
static UWORD slot_next(UBYTE s) { return s+1>=gamestate.num_sides ? 0:s+1; }
static UBYTE wall_at(WORD inner,WORD outer,UWORD slot) {
    for (UWORD i=0;i<n_active;++i)
        if (spans[i].slot==slot && spans[i].inner==inner && spans[i].outer==outer) return 1;
    return 0;
}

// Draw a wall's outline as fill seeds, but:
//  - cull entirely if the whole trapezoid is off the top, bottom or left (those
//    contribute nothing to the fill; NOT the right edge, which still needs its
//    x=XMAX fill-fixup toggle).
//  - skip a radial edge where an angular neighbour at the same dist draws the
//    identical edge - those cancel under XOR anyway.
// A closed ring of 5 walls near spawn: ~2 culled, the rest 12 edges not 20.
static void draw_wall(const PcSpan* w, void* buf) {
    UWORD a0 = gamestate.field_angle - (UWORD)w->slot * gamestate.segment_angle;
    UWORD a1 = w->slot+1==gamestate.num_sides ? gamestate.field_angle : a0-gamestate.segment_angle;
    WORD r0 = zscale(w->inner);
    WORD r1 = zscale(w->outer);
    WORD x00, y00, x10, y10, x11, y11, x01, y01;
    pt(a0, r0, &x00, &y00);
    pt(a1, r0, &x10, &y10);
    pt(a1, r1, &x11, &y11);
    pt(a0, r1, &x01, &y01);

    if ((y00 < 0    && y10 < 0    && y11 < 0    && y01 < 0)   ||
        (y00 > YMAX  && y10 > YMAX  && y11 > YMAX  && y01 > YMAX) ||
        (x00 < 0    && x10 < 0    && x11 < 0    && x01 < 0))
        return;

    blit_clipped_line_onedot(x00, y00, x10, y10, 0, buf); // inner chord
    blit_clipped_line_onedot(x01, y01, x11, y11, 0, buf); // outer chord

    if (!wall_at(w->inner, w->outer, slot_prev(w->slot)))
        blit_clipped_line_onedot(x01, y01, x00, y00, 0, buf); // leading radial
    if (!wall_at(w->inner, w->outer, slot_next(w->slot)))
        blit_clipped_line_onedot(x11, y11, x10, y10, 0, buf); // trailing radial
}

#define SPOKE_OUTER     190  // spoke length, world units (pre-zoom)
#define SPOKE_OUTER_MAX 95  // safety clamp on the ZOOMED (screen-space) length

void render_spokes(void* buf) {
    blit_line_mode(); // re-arm line-mode registers after the fill

    WORD inner = zscale(HUB_RADIUS);
    WORD outer = zscale(SPOKE_OUTER);
    // Raw blit_line requires on-screen endpoints, including title zoom.
    if (outer > SPOKE_OUTER_MAX) outer = SPOKE_OUTER_MAX;
    UWORD a = gamestate.field_angle;
    for (WORD i = 0; i < gamestate.num_sides; i++) {
        WORD sx, sy, ex, ey;
        polar_to_cartesian(a, (UWORD)inner, &sx, &sy);
        polar_to_cartesian(a, (UWORD)outer, &ex, &ey);
        blit_line((UWORD)(CX + sx), (UWORD)(CY + sy),
                  (UWORD)(CX + ex), (UWORD)(CY + ey), buf);
        a -= gamestate.segment_angle;
    }
}

void render_game(void* buf) {
    ox = game_shake_x();
    oy = game_shake_y();
    zoom = gamestate.draw_distance;
    if (zoom < 64) zoom = 64; // guard against a collapsed view

    blit_line_mode();
    blit_fill_reset();

    GameMode m = game_mode();

    if (m == MODE_PLAYING || m == MODE_DEAD || m == MODE_GAMEOVER) {
        n_active=pc_project_spans(&game_world,gamestate.num_sides,spans);
        for (UWORD k=0;k<n_active;++k) draw_wall(&spans[k],buf);
    }
#if BUILD_DEBUG
    custom->color[0] = 0x033; // cyan: wall seeds done
#endif

    draw_hub(buf);

    draw_player(buf);
}

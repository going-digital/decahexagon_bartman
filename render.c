#include "render.h"
#include "config.h"
#include "blitter.h"
#include "trig.h"
#include "game.h"

#define CX (SCREEN_WIDTH / 2)
#define CY (SCREEN_HEIGHT / 2)

#define PLAYER_TIP_LEN   6      // how far the nose pokes past PLAYER_RADIUS
#define PLAYER_HALF_ANG  1500   // half the triangle's angular width

static WORD  ox, oy;  // camera shake offset applied this frame
static UWORD zoom;    // Q8 camera zoom applied this frame (ZOOM_ONE = 1.0)

// (worldAngle, radius) -> screen pixel, with zoom and shake applied.
static void pt(UWORD ang, WORD r, WORD* sx, WORD* sy) {
    WORD x, y;
    WORD rr = (WORD)(((LONG)r * zoom) >> 8);
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
    WORD xs[NUM_SIDES], ys[NUM_SIDES];
    UWORD a = gamestate.field_angle;
    for (WORD i = 0; i < NUM_SIDES; i++) {
        pt(a, HUB_RADIUS, &xs[i], &ys[i]);
        a += gamestate.segment_angle;
    }
    poly(xs, ys, NUM_SIDES, buf);
}

static void draw_player(void* buf) {
    UWORD a = gamestate.field_angle + gamestate.player_angle;
    WORD xs[3], ys[3];
    pt(a, PLAYER_RADIUS + PLAYER_TIP_LEN, &xs[0], &ys[0]);
    pt(a - PLAYER_HALF_ANG, PLAYER_RADIUS, &xs[1], &ys[1]);
    pt(a + PLAYER_HALF_ANG, PLAYER_RADIUS, &xs[2], &ys[2]);
    poly(xs, ys, 3, buf);
}

static void draw_wall(const Wall* w, void* buf) {
    UWORD a0 = gamestate.field_angle + (UWORD)w->slot * gamestate.segment_angle;
    UWORD a1 = a0 + gamestate.segment_angle;
    WORD r0 = w->dist;
    WORD r1 = w->dist + WALL_THICKNESS;
    WORD xs[4], ys[4];
    pt(a0, r0, &xs[0], &ys[0]);
    pt(a1, r0, &xs[1], &ys[1]);
    pt(a1, r1, &xs[2], &ys[2]);
    pt(a0, r1, &xs[3], &ys[3]);
    poly(xs, ys, 4, buf);
}

void render_game(void* buf) {
    ox = game_shake_x();
    oy = game_shake_y();
    zoom = gamestate.draw_distance;
    if (zoom < 64) zoom = 64; // guard against a collapsed view

    blit_line_mode();

    GameMode m = game_mode();

    if (m == MODE_PLAYING || m == MODE_DEAD) {
        for (WORD i = 0; i < MAX_WALLS; i++)
            if (walls[i].active) draw_wall(&walls[i], buf);
    }

    draw_hub(buf);

    if (m != MODE_ATTRACT)
        draw_player(buf);
}

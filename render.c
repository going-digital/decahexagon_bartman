#include "render.h"
#include "config.h"
#include "blitter.h"
#include "trig.h"
#include "game.h"
#include "pc_projection.h"
#include "player_shape.h"
#include "system.h"

#define CX (SCREEN_WIDTH / 2)
#define CY (SCREEN_HEIGHT / 2)

#define PLAYER_TIP_LEN   6      // how far the nose pokes past PLAYER_RADIUS
#define PLAYER_HALF_ANG  1500   // half the triangle's angular width

static WORD  ox, oy;  // camera shake offset applied this frame
static RenderScene scene;
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

// Sector boundaries share directions across all walls, hub and spokes.
static void slot_pt(UWORD slot, WORD radius, WORD* x, WORD* y) {
    direction_to_cartesian(frame_sin[slot], frame_cos[slot], (UWORD)radius, x, y);
    *x += CX + ox;
    *y += CY + oy;
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
    WORD xs[MAX_NUM_SIDES], ys[MAX_NUM_SIDES]; // capacity; scene.num_sides (runtime) says how many are used
    WORD rr = zscale(HUB_RADIUS + scene.pulse);
    UBYTE n = scene.num_sides;
    for (WORD i = 0; i < n; i++) {
        slot_pt(i, rr, &xs[i], &ys[i]);
    }
    poly(xs, ys, n, buf);
}

/* One pair per playfield buffer. Never edit data still being read by DMA. */
typedef UWORD PlayerSpritePair[2][2 + PLAYER_SHAPE_SIZE * 2 + 2];
static PlayerSpritePair *player_sprites;
static const UWORD blank_player[4] __attribute__((section(".MEMF_CHIP")))={0,0,0,0};
static UWORD player_sprite_slot;
#define PLAYER_POSES 128
#define PLAYER_POSE_WORDS (16 * 2 + 2)
static UWORD (*player_poses)[PLAYER_POSE_WORDS];
static struct { WORD x,y; UWORD width,height; } player_pose_bounds[PLAYER_POSES];
static const UWORD *player_cached_pixels;

const UWORD* render_player_sprite(unsigned column) {
    return player_sprites?player_sprites[player_sprite_slot][column]:blank_player;
}

const UWORD* render_player_pixels(unsigned column) {
    if (!column && player_cached_pixels) return player_cached_pixels;
    return render_player_sprite(column)+2;
}

void render_free(void) {
    if (player_sprites) GameFreeChip(player_sprites,3*sizeof(*player_sprites));
    player_sprites=0;
    if (player_poses) GameFreeChip(player_poses,PLAYER_POSES*sizeof(*player_poses));
    player_poses=0;
    player_cached_pixels=0;
}

static void player_header(UWORD *sprite, WORD x, WORD y, UWORD height) {
    UWORD h=DISPLAY_HW_X+x, v=DISPLAY_HW_Y+y, stop=v+height;
    sprite[0]=((v&255)<<8)|((h>>1)&255);
    sprite[1]=((stop&255)<<8)|(((v>>8)&1)<<2)|(((stop>>8)&1)<<1)|(h&1);
}

static void player_prerender(void) {
    player_poses=GameAllocChip(PLAYER_POSES*sizeof(*player_poses));
    if (!player_poses) return;
    for (unsigned i=0;i<PLAYER_POSES;++i) {
        UWORD a=i<<9;
        WORD xs[3],ys[3],bx,by;
        polar_to_cartesian(a,((PLAYER_RADIUS+PLAYER_TIP_LEN)*GAMEPLAY_ZOOM)>>8,&xs[0],&ys[0]);
        polar_to_cartesian(a-PLAYER_HALF_ANG,(PLAYER_RADIUS*GAMEPLAY_ZOOM)>>8,&xs[1],&ys[1]);
        polar_to_cartesian(a+PLAYER_HALF_ANG,(PLAYER_RADIUS*GAMEPLAY_ZOOM)>>8,&xs[2],&ys[2]);
        PlayerShape shape;
        if (!player_shape_build(&shape,xs,ys) || shape.width>16 || shape.height>16) {
            GameFreeChip(player_poses,PLAYER_POSES*sizeof(*player_poses));
            player_poses=0;
            return; // Geometry changes retain the live renderer safely.
        }
        polar_to_cartesian(a,(PLAYER_RADIUS*GAMEPLAY_ZOOM)>>8,&bx,&by);
        player_pose_bounds[i].x=shape.x-bx;
        player_pose_bounds[i].y=shape.y-by;
        player_pose_bounds[i].width=shape.width;
        player_pose_bounds[i].height=shape.height;
        for (unsigned y=0;y<shape.height;++y)
            player_poses[i][y*2]=player_poses[i][y*2+1]=shape.rows[y][0];
        // AllocMem cleared the terminator. These DMA pixels never change.
    }
}

static unsigned player_use_pose(UWORD a) {
    if (zoom!=GAMEPLAY_ZOOM || !player_sprites || !player_poses) return 0;
    unsigned pose=((UWORD)(a+256))>>9; // nearest 2.8125-degree orientation
    WORD x,y,dx,dy;
    // Keep orbit and pulse at their original angular resolution. Only the
    // tiny triangle's orientation is quantised, not its screen position.
    pt(a,(PLAYER_RADIUS*GAMEPLAY_ZOOM)>>8,&x,&y);
    polar_to_cartesian(a,(UWORD)zscale(scene.pulse),&dx,&dy);
    x+=dx+player_pose_bounds[pose].x;
    y+=dy+player_pose_bounds[pose].y;
    if (x<0 || y<PLAYER_SPRITE_MIN_Y ||
        x+player_pose_bounds[pose].width>SCREEN_WIDTH ||
        y+player_pose_bounds[pose].height>SCREEN_HEIGHT) return 0;
    player_header(player_sprites[player_sprite_slot][0],x,y,player_pose_bounds[pose].height);
    player_cached_pixels=player_poses[pose];
    return 1;
}

void render_player(void* buf) {
    UWORD a = scene.field_angle + scene.player_angle;
    player_sprite_slot = player_sprite_slot==2?0:player_sprite_slot+1;
    player_cached_pixels=0;
    if (player_sprites) for (unsigned ch=0;ch<2;++ch) {
        player_sprites[player_sprite_slot][ch][0]=0;
        player_sprites[player_sprite_slot][ch][1]=0;
    }
    if(game_mode()==MODE_ENDING || game_ending_complete())return;
    if (player_use_pose(a)) return;
    WORD rt = zscale(PLAYER_RADIUS + PLAYER_TIP_LEN);
    WORD rb = zscale(PLAYER_RADIUS);
    WORD xs[3], ys[3];
    pt(a, rt, &xs[0], &ys[0]);
    pt(a - PLAYER_HALF_ANG, rb, &xs[1], &ys[1]);
    pt(a + PLAYER_HALF_ANG, rb, &xs[2], &ys[2]);
    /* PC moves the player's centre with the pulse; its triangle does not
     * widen as the orbit expands. Translate every vertex by one vector. */
    WORD dx,dy;
    polar_to_cartesian(a,(UWORD)zscale(scene.pulse),&dx,&dy);
    for(unsigned i=0;i<3;++i) {xs[i]+=dx;ys[i]+=dy;}
    PlayerShape shape;
    if (!player_shape_build(&shape,xs,ys)) return;
    /* Channels 6/7 belong to the HUD until its banner has finished. Keep a
     * solid marker even if future shake/pulse changes put it in that band,
     * or beyond the viewport where sprite coordinates could wrap around. */
    if (!player_sprites || shape.y<PLAYER_SPRITE_MIN_Y || shape.x<0 ||
        shape.x+shape.width>SCREEN_WIDTH || shape.y+shape.height>SCREEN_HEIGHT) {
        blit_wait();
        for (unsigned y=0;y<shape.height;++y) {
            WORD sy=shape.y+y;
            if (sy<0 || sy>=SCREEN_HEIGHT) continue;
            for (unsigned x=0;x<shape.width;++x) {
                WORD sx=shape.x+x;
                if (sx>=0 && sx<SCREEN_WIDTH &&
                    (shape.rows[y][x>>4] & (0x8000u>>(x&15))))
                    ((UBYTE*)buf)[sy*SCREEN_WIDTH_BYTES+(sx>>3)] |= 0x80u>>(sx&7);
            }
        }
        return;
    }
    for (unsigned ch=0;ch<2;++ch) {
        if (ch && shape.width<=16) continue;
        UWORD *sprite=player_sprites[player_sprite_slot][ch];
        player_header(sprite,shape.x+ch*16,shape.y,shape.height);
        for (unsigned y=0;y<shape.height;++y)
            sprite[2+y*2]=sprite[3+y*2]=shape.rows[y][ch]; // colour 31
        sprite[2+shape.height*2]=sprite[3+shape.height*2]=0;
    }
}

// Render-only unions keep overlapping same-slot walls solid under XOR fill.
static PcSpan spans[PC_WALL_CAPACITY];
static UWORD n_active;
static UBYTE shared_edges[PC_WALL_CAPACITY];

// Draw a wall's outline as fill seeds, but:
//  - cull entirely if the whole trapezoid is off the top, bottom or left (those
//    contribute nothing to the fill; NOT the right edge, which still needs its
//    x=XMAX fill-fixup toggle).
//  - skip a radial edge where an angular neighbour at the same dist draws the
//    identical edge - those cancel under XOR anyway.
// A closed ring of 5 walls near spawn: ~2 culled, the rest 12 edges not 20.
static void draw_wall(const PcSpan* w, UBYTE shared, void* buf) {
    UWORD next = w->slot+1==scene.num_sides ? 0 : w->slot+1;
    WORD r0 = zscale(w->inner + scene.pulse);
    WORD r1 = zscale(w->outer + scene.pulse);
    WORD x00, y00, x10, y10, x11, y11, x01, y01;
    slot_pt(w->slot, r0, &x00, &y00);
    slot_pt(next, r0, &x10, &y10);
    slot_pt(next, r1, &x11, &y11);
    slot_pt(w->slot, r1, &x01, &y01);

    if ((y00 < 0    && y10 < 0    && y11 < 0    && y01 < 0)   ||
        (y00 > YMAX  && y10 > YMAX  && y11 > YMAX  && y01 > YMAX) ||
        (x00 < 0    && x10 < 0    && x11 < 0    && x01 < 0))
        return;

    blit_clipped_line_onedot(x00, y00, x10, y10, 0, buf); // inner chord
    blit_clipped_line_onedot(x01, y01, x11, y11, 0, buf); // outer chord

    if (!(shared & PC_SPAN_PREV))
        blit_clipped_line_onedot(x01, y01, x00, y00, 0, buf); // leading radial
    if (!(shared & PC_SPAN_NEXT))
        blit_clipped_line_onedot(x11, y11, x10, y10, 0, buf); // trailing radial
}

// Fixed-centre viewport intersections: ordinary RAM, four bytes per angle.
static WORD spoke_end[1024][2];

static void spoke_endpoint(UWORD a, WORD cx, WORD cy, WORD* x, WORD* y) {
    WORD dx, dy, ex;
    LONG ey;
    polar_to_cartesian(a, 1024, &dx, &dy);
    if (dx) {
        ex = dx > 0 ? XMAX : 0;
        ey = cy + (LONG)(ex - cx) * dy / dx;
    } else {
        ex = cx;
        ey = dy > 0 ? YMAX : 0;
    }
    if (ey < 0 || ey > YMAX) {
        ey = dy > 0 ? YMAX : 0;
        ex = cx + (LONG)(ey - cy) * dx / dy;
    }
    *x = ex;
    *y = (WORD)ey;
}

void render_init(void) {
    player_sprites=GameAllocChip(3*sizeof(*player_sprites));
    player_sprite_slot=0;
    player_prerender();
    for (UWORD i = 0; i < 1024; ++i)
        spoke_endpoint(i << 6, CX, CY, &spoke_end[i][0], &spoke_end[i][1]);
}

void render_spokes(void* buf) {
    blit_line_mode(); // re-arm line-mode registers after the fill

    WORD inner = zscale(HUB_RADIUS + scene.pulse);
    UWORD a = scene.field_angle;
    for (WORD i = 0; i < scene.num_sides; i++) {
        WORD sx, sy, ex, ey;
        slot_pt(i, inner, &sx, &sy);
        if (ox || oy) {
            // Preserve camera-offset behaviour if shake is enabled later.
            spoke_endpoint(a, CX + ox, CY + oy, &ex, &ey);
        } else {
            ex = spoke_end[a >> 6][0];
            ey = spoke_end[a >> 6][1];
        }
        // Raw blit_line requires on-screen endpoints. A zoomed hub corner
        // outside the viewport has no visible outward spoke.
        if (sx >= 0 && sx <= XMAX && sy >= 0 && sy <= YMAX)
            blit_line((UWORD)sx, (UWORD)sy, (UWORD)ex, (UWORD)ey, buf);
        a -= scene.segment_angle;
    }
}

void render_scene(void* buf,const RenderScene *input,const PcWorld *world) {
    scene=*input;
    ox = scene.shake_x;
    oy = scene.shake_y;
    zoom = scene.draw_distance;
    if (zoom < 64) zoom = 64; // guard against a collapsed view
    UWORD angle = scene.field_angle;
    for (UWORD i=0;i<scene.num_sides;++i) {
        UWORD index = angle >> 6;
        frame_sin[i] = sin_table[index];
        frame_cos[i] = cos_table[index];
        angle -= scene.segment_angle;
    }

    blit_line_mode();
    blit_fill_reset();

    if (world) {
        n_active=game_mode()==MODE_ENDING ?
            pc_project_ending_spans(world,scene.num_sides,spans):
            pc_project_spans(world,scene.num_sides,spans);
        pc_span_shared_edges(spans,n_active,scene.num_sides,shared_edges);
        for (UWORD k=0;k<n_active;++k) draw_wall(&spans[k],shared_edges[k],buf);
    }
#if BUILD_DEBUG
    custom->color[0] = 0x033; // cyan: wall seeds done
#endif

    draw_hub(buf);

}

/* Ordinary gameplay and endings share the same renderer. Only scene inputs
 * and wall scheduling differ; sprite/fill/display ownership stays unchanged. */
void render_game(void *buf) {
    RenderScene input={gamestate.field_angle,gamestate.segment_angle,
        gamestate.player_angle,gamestate.draw_distance,gamestate.pulse,
        gamestate.num_sides,game_shake_x(),game_shake_y()};
    GameMode m=game_mode();
    const PcWorld *world=(m==MODE_PLAYING || m==MODE_DEAD || m==MODE_GAMEOVER || m==MODE_ENDING) ? &game_world:0;
    render_scene(buf,&input,world);
}

#!/usr/bin/env python3
"""Check atlas selection/lifetime using the actual render.c player functions.
Host trigonometry stands in for the target helper; emulator runs exercise the
actual 68000 helper. Exact-pose comparisons use the live renderer as an oracle.
"""
from pathlib import Path
import os, subprocess, tempfile
root=Path(__file__).resolve().parents[1]
s=(root/'render.c').read_text()
s=s[s.index('/* One pair per playfield buffer.'):s.index('// Render-only unions')]
s=s.replace('__attribute__((section(".MEMF_CHIP")))','')
header=r'''
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "player_shape.h"
#include "render.h"
#include "view_scale.h"
#define PLAYER_RADIUS 54
#define PLAYER_TIP_LEN 6
#define PLAYER_HALF_ANG 1500
#define GAMEPLAY_ZOOM 128
#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 200
#define SCREEN_WIDTH_BYTES 40
#define DISPLAY_HW_X 129
#define DISPLAY_HW_Y 72
#define MEMF_CHIP 1
#define MEMF_CLEAR 2
#define CX 160
#define CY 100
typedef uint8_t UBYTE;
static UWORD zoom=128;
static WORD ox,oy;
static struct {UWORD field_angle,player_angle,pulse;} gamestate;
static unsigned polar_calls,alloc_fail,allocations;
static void *AllocMem(unsigned size,unsigned flags) {
 (void)flags;if(alloc_fail)return 0;++allocations;return calloc(1,size);
}
static void FreeMem(void *p,unsigned size){(void)size;assert(allocations);--allocations;free(p);}
static void blit_wait(void){}
static WORD zscale(WORD r){return ((unsigned)r*zoom)>>8;}
static void polar_to_cartesian(UWORD a,UWORD r,WORD *x,WORD *y){
 ++polar_calls;double angle=(a>>6)*6.283185307179586/1024;
 *x=(WORD)(((int32_t)view_scale_x((WORD)floor(cos(angle)*16384))*r)>>14);
 *y=(WORD)(((int32_t)view_scale_y((WORD)floor(sin(angle)*16384))*r)>>14);
}
static void pt(UWORD a,WORD r,WORD *x,WORD *y){polar_to_cartesian(a,r,x,y);*x+=CX+ox;*y+=CY+oy;}
'''
footer=r'''
int main(void){
 static UBYTE plane[8000];
 player_sprites=AllocMem(3*sizeof(*player_sprites),MEMF_CHIP|MEMF_CLEAR);
 player_prerender();assert(player_poses);
 UWORD snapshot[PLAYER_POSES][PLAYER_POSE_WORDS];memcpy(snapshot,player_poses,sizeof(snapshot));
 for(unsigned i=0;i<128;++i) {
  gamestate.field_angle=i<<9;gamestate.pulse=i%48;polar_calls=0;
  render_player(plane);assert(player_cached_pixels==player_poses[i]);assert(polar_calls==2);
  UWORD header[2]={render_player_sprite(0)[0],render_player_sprite(0)[1]};
  UWORD pixels[PLAYER_POSE_WORDS];memcpy(pixels,render_player_pixels(0),sizeof(pixels));
  UWORD (*saved)[PLAYER_POSE_WORDS]=player_poses;player_poses=0;
  render_player(plane);assert(!player_cached_pixels);
  assert(!memcmp(header,render_player_sprite(0),sizeof(header)));
  unsigned words=(player_pose_bounds[i].height*2+2);
  assert(!memcmp(pixels,render_player_pixels(0),words*sizeof(UWORD)));
  player_poses=saved;
 }
 gamestate.pulse=0;
 for(unsigned a=0;a<65536;++a) {
  gamestate.field_angle=a;render_player(plane);
  assert(player_cached_pixels==player_poses[((UWORD)(a+256))>>9]);
  assert(!render_player_sprite(1)[0] && !render_player_sprite(1)[1]);
 }
 assert(!memcmp(snapshot,player_poses,sizeof(snapshot))); // DMA images stayed immutable
 for(zoom=129;zoom<=320;++zoom){render_player(plane);assert(!player_cached_pixels);}
 zoom=128;oy=-100;render_player(plane);assert(!player_cached_pixels);oy=0;
 render_free();assert(!allocations && !player_cached_pixels);
 alloc_fail=1;player_prerender();assert(!player_poses);
 render_player(plane);assert(!player_cached_pixels);assert(!allocations);
 puts("Player atlas: 128 live-render matches, 65,536 angles, immutable pixels, zoom/viewport/allocation fallbacks passed");
}
'''
with tempfile.TemporaryDirectory() as tmp:
 p=Path(tmp);(p/'exec').mkdir();(p/'exec/types.h').write_text('typedef unsigned short UWORD; typedef short WORD;\n')
 (p/'check.c').write_text(header+s+footer)
 subprocess.run([os.environ.get('HOST_CC','cc'),'-std=c99','-O2','-Wall','-Wextra','-Werror','-I'+str(p),'-I'+str(root),str(p/'check.c'),str(root/'player_shape.c'),'-lm','-o',str(p/'check')],check=True)
 subprocess.run([str(p/'check')],check=True,timeout=15)

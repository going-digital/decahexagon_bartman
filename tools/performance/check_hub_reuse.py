#!/usr/bin/env python3
"""Compare production hub/spoke command streams before/after endpoint reuse."""
from pathlib import Path
import subprocess,json
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_hub'
def function(s,name):
 start=s.index(name+'(');start=s.rfind('\n',0,start)+1;end=s.index('{',start)+1;depth=1
 while depth:
  depth+=(s[end]=='{')-(s[end]=='}');end+=1
 return s[start:end]
header=r'''
#include <stdint.h>
#include <string.h>
#include <math.h>
typedef int16_t WORD;typedef uint16_t UWORD;typedef uint8_t UBYTE;typedef int32_t LONG;
#define MAX_NUM_SIDES 6
#define CX 160
#define CY 100
#define XMAX 319
#define YMAX 199
#define HUB_RADIUS 40
static WORD ox,oy,frame_sin[6],frame_cos[6],spoke_end[1024][2],hub_x[6],hub_y[6];
static UWORD zoom;
static struct {UWORD field_angle,segment_angle,pulse;UBYTE num_sides;} scene;
static int trace[80],used;
static unsigned muluw(UWORD a,UWORD b){return (unsigned)a*b;}
static void direction_to_cartesian(WORD sine,WORD cosine,UWORD r,WORD*x,WORD*y){
 *x=(WORD)(((int32_t)cosine*(int16_t)r)>>14);*y=(WORD)(((int32_t)sine*(int16_t)r)>>14);
}
static void polar_to_cartesian(UWORD a,UWORD r,WORD*x,WORD*y){
 double t=a*6.283185307179586/65536.;
 direction_to_cartesian((WORD)(sin(t)*16384),(WORD)(cos(t)*16384),r,x,y);
}
static void emit(int kind,int x0,int y0,int x1,int y1){
 trace[used++]=kind;trace[used++]=x0;trace[used++]=y0;trace[used++]=x1;trace[used++]=y1;
}
static void blit_clipped_line_onedot(WORD a,WORD b,WORD c,WORD d,UWORD angle,void*buf){(void)angle;(void)buf;emit(0,a,b,c,d);}
static void blit_line(UWORD a,UWORD b,UWORD c,UWORD d,void*buf){(void)buf;emit(1,a,b,c,d);}
static void blit_line_mode(void){}
'''
footer=r'''
void run_scene(unsigned seed,int *result){
 scene.num_sides=3+seed%4;scene.field_angle=(UWORD)(seed*173);
 scene.segment_angle=(UWORD)(65536u/scene.num_sides+seed%113);
 scene.pulse=seed%101;zoom=64+seed%449;
 ox=(seed%5)*9-18;oy=(seed%7)*5-15;
 for(unsigned i=0;i<6;++i){
  UWORD a=scene.field_angle-i*scene.segment_angle;
  double t=a*6.283185307179586/65536.;
  frame_sin[i]=(WORD)(sin(t)*12288);frame_cos[i]=(WORD)(cos(t)*16384);
 }
 for(unsigned i=0;i<1024;++i){spoke_end[i][0]=i%320;spoke_end[i][1]=i%200;}
 memset(trace,0,sizeof(trace));used=0;draw_hub(0);render_spokes(0);
 memcpy(result,trace,sizeof(trace));
}
'''
for variant,path in [('before',out/'before_render.c'),('after',root/'render.c')]:
 s=path.read_text();code=header+'\n'.join(function(s,n) for n in ('zscale','slot_pt','poly','draw_hub','spoke_endpoint','render_spokes'))+footer
 (out/(variant+'.c')).write_text(code)
 subprocess.run(['cc','-O2','-Drun_scene='+variant+'_scene','-Drender_spokes='+variant+'_spokes','-c',str(out/(variant+'.c')),'-o',str(out/(variant+'.o'))],check=True)
(out/'check.c').write_text('''#include <assert.h>
#include <string.h>
void before_scene(unsigned,int*);void after_scene(unsigned,int*);
int main(void){int a[80],b[80];for(unsigned i=0;i<131072;++i){before_scene(i,a);after_scene(i,b);assert(!memcmp(a,b,sizeof(a)));}return 0;}
''')
subprocess.run(['cc','-O2',str(out/'check.c'),str(out/'before.o'),str(out/'after.o'),'-lm','-o',str(out/'check')],check=True)
subprocess.run([str(out/'check')],check=True)
r=dict(cases=131072,mismatches=0,scope='Host comparison of extracted production hub/spoke routines and line command streams, including culling. Varies sides3..6, angles, zoom64..512, pulse0..100, camera offsets and successive scenes. Deterministic host trig substitutes for target assembly; this verifies reuse semantics, not target trig accuracy or DMA pixels.')
(root/'docs/HUB_REUSE_ACCURACY.json').write_text(json.dumps(r,indent=2)+'\n');print(r)

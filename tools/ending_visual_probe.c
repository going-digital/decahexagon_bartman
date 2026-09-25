/* Host visualization of actual C transforms; synthetic planar scene. */
#include "../pc_ending_affine.h"
#include <math.h>
#include <stdio.h>
int main(void) {
 const int tilts[]={0,20,30};
 for(unsigned t=0;t<3;t++)for(int o=-20;o<=20;o+=20)for(int rot=0;rot<60;rot+=5) {
  PcEndingView v={tilts[t],o,600,160,100,300,300};PcEndingAffine m;
  if(!pc_ending_affine_prepare(&m,&v))return 1;
  printf("{\"tilt\":%d,\"otis\":%d,\"rotation\":%d,\"polygons\":[",v.tilt,o,rot);
  for(int wall=0;wall<9;wall++) {
   int slot=wall%6,inner=wall<6 ? 220:420,outer=inner+50;
   if(wall)putchar(',');printf("[");
   for(int corner=0;corner<4;corner++) {
    int r=corner<2 ? inner:outer;
    double angle=(rot+(slot+(corner==1 || corner==2))*60)*acos(-1.0)/180;
    int16_t x=(int16_t)lround(cos(angle)*r),y=(int16_t)lround(sin(angle)*r);
    int32_t ax,ay,px,py;
    if(!pc_ending_affine_point(&m,x,y,&ax,&ay) || !pc_ending_project_wide(&v,x,y,0,&px,&py))return 1;
    if(corner)putchar(',');printf("[%d,%d,%d,%d]",ax,ay,px,py);
   }printf("]");
  }puts("]}");
 }
}

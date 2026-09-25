#include "../pc_ending_affine.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
int main(void) {
 unsigned count=0;double pi=acos(-1.0);
 for(int t=0;t<=30;t+=10)for(int o=-20;o<=20;o+=10) {
  PcEndingView v={t,o,600,160,100,300,300};PcEndingAffine m;
  assert(pc_ending_affine_prepare(&m,&v));
  for(int x=-600;x<=600;x+=100)for(int y=-600;y<=600;y+=100) {
   int32_t sx,sy;assert(pc_ending_affine_point(&m,x,y,&sx,&sy));
   double a=t*pi/180,b=o*pi/180,scale=300.0/(600-5*t);
   double ex=160+scale*(x*cos(b)-y*sin(a)*sin(b));
   double ey=100+scale*y*cos(a);
   assert(fabs(sx-ex)<1.5 && fabs(sy-ey)<1.5);count++;
  }
 }
 PcEndingView v={0,0,0,0,0,300,300};PcEndingAffine m={1,2,3,4,5};
 assert(!pc_ending_affine_prepare(&m,&v) && m.a==1 && m.y==5);
 PcEndingAffineCache cache={0};v.depth=600;
 const PcEndingAffine *cached=pc_ending_affine_cached(&cache,&v);assert(cached && cache.valid);
 PcEndingAffine original=*cached;
 v.tilt=30;v.otis=20;
 assert(pc_ending_affine_cached(&cache,&v)->b==0);
 assert(cache.matrix.a==original.a && cache.matrix.d==original.d);
 assert(cache.view.tilt==0 && cache.view.otis==0);
 assert(pc_ending_affine_cached(&cache,&v)==cached && cached->a==original.a);
 v.centre_x=123;assert(pc_ending_affine_cached(&cache,&v)->x==123);
 v.depth=0;assert(!pc_ending_affine_cached(&cache,&v) && !cache.valid);
 v.depth=600;assert(pc_ending_affine_cached(&cache,&v) && cache.valid);
 PcEndingVertex quad[4]={{-100,-100,0},{100,-100,0},{100,100,0},{-100,100,0}};
 int16_t xs[4]={1,2,3,4},ys[4]={5,6,7,8};
 assert(pc_ending_affine_quad(&cache.matrix,quad,xs,ys));
 int16_t old=xs[0];quad[3].z=1;
 assert(!pc_ending_affine_quad(&cache.matrix,quad,xs,ys) && xs[0]==old);
 quad[3].z=0;quad[3].x=8193;
 assert(!pc_ending_affine_quad(&cache.matrix,quad,xs,ys) && xs[0]==old);
 quad[3].x=-100;PcEndingAffine large={32767,0,32767,0,0};quad[3].x=8192;
 assert(!pc_ending_affine_quad(&large,quad,xs,ys) && xs[0]==old);
 puts("Affine camera: 3380 planar cases within 1.5 pixels of analytic affine transform");assert(count==3380);
}

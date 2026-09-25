#include "pc_ending_affine.h"
static const int16_t sine[91]={
0,286,572,857,1143,1428,1713,1997,2280,2563,2845,3126,3406,3686,3964,4240,4516,4790,5063,5334,5604,5872,6138,6402,6664,6924,7182,7438,7692,7943,8192,8438,8682,8923,9162,9397,9630,9860,10087,10311,10531,10749,10963,11174,11381,11585,11786,11982,12176,12365,12551,12733,12911,13085,13255,13421,13583,13741,13894,14044,14189,14330,14466,14598,14726,14849,14968,15082,15191,15296,15396,15491,15582,15668,15749,15826,15897,15964,16026,16083,16135,16182,16225,16262,16294,16322,16344,16362,16374,16382,16384
};
static int16_t sn(int a) {
 a%=360;if(a<0)a+=360;
 if(a<=90)return sine[a];if(a<=180)return sine[180-a];
 if(a<=270)return -sine[a-180];return -sine[360-a];
}
/* Dedicated production setup: keeps reference trigonometry out of the link. */
static int prepare_flat(PcEndingAffine *out,const PcEndingView *v) {
 if(v->depth<=0 || v->focal_x>4096 || v->focal_y>4096)return 0;
 int32_t a=(int32_t)v->focal_x*4096/v->depth;
 int32_t d=v->focal_x==v->focal_y ? a:(int32_t)v->focal_y*4096/v->depth;
 if(a>32767 || d>32767)return 0;
 *out=(PcEndingAffine){a,0,d,v->centre_x,v->centre_y};return 1;
}
int pc_ending_affine_prepare(PcEndingAffine *out,const PcEndingView *v) {
 if(!out || !v || v->tilt< -90 || v->tilt>90 || v->focal_x>4096 || v->focal_y>4096)return 0;
 int32_t depth=(int32_t)v->depth-5*v->tilt;
 if(depth<=0)return 0;
 if(!v->tilt && !v->otis)return prepare_flat(out,v);
 int16_t co=sn(90+v->otis),so=sn(v->otis),st=sn(-v->tilt),ct=sn(90-v->tilt);
 int32_t shear=((int32_t)st*so)/16384;
 int32_t a=((int32_t)co*v->focal_x)/depth/4;
 int32_t b=(shear*v->focal_x)/depth/4;
 int32_t d=((int32_t)ct*v->focal_y)/depth/4;
 if(a< -32767 || a>32767 || b< -32767 || b>32767 || d< -32767 || d>32767)return 0;
 *out=(PcEndingAffine){a,b,d,v->centre_x,v->centre_y};return 1;
}
int pc_ending_affine_point(const PcEndingAffine *m,int16_t x,int16_t y,int32_t *sx,int32_t *sy) {
 if(!m || !sx || !sy || x< -8192 || x>8192 || y< -8192 || y>8192)return 0;
 *sx=((int32_t)m->a*x+(m->b ? (int32_t)m->b*y:0))/4096+m->x;
 *sy=((int32_t)m->d*y)/4096+m->y;return 1;
}
const PcEndingAffine *pc_ending_affine_cached(PcEndingAffineCache *cache,const PcEndingView *v) {
 if(!cache || !v)return 0;
 /* User-selected Amiga camera: discard phase tilt/Otis before cache lookup. */
 PcEndingView flat=*v;flat.tilt=flat.otis=0;v=&flat;
 const PcEndingView *p=&cache->view;
 if(cache->valid && p->tilt==v->tilt && p->otis==v->otis && p->depth==v->depth &&
    p->centre_x==v->centre_x && p->centre_y==v->centre_y &&
    p->focal_x==v->focal_x && p->focal_y==v->focal_y)return &cache->matrix;
 cache->valid=0;
 if(!prepare_flat(&cache->matrix,v))return 0;
 cache->view=*v;cache->valid=1;return &cache->matrix;
}
int pc_ending_affine_quad(const PcEndingAffine *m,const PcEndingVertex vertices[4],
                          int16_t xs[4],int16_t ys[4]) {
 if(!m || !vertices || !xs || !ys)return 0;
 int16_t tx[4],ty[4];
 for(unsigned i=0;i<4;i++) {
  int32_t x,y;
  if(vertices[i].z || !pc_ending_affine_point(m,vertices[i].x,vertices[i].y,&x,&y) ||
     x< -8191 || x>8191 || y< -8191 || y>8191)return 0;
  tx[i]=(int16_t)x;ty[i]=(int16_t)y;
 }
 for(unsigned i=0;i<4;i++){xs[i]=tx[i];ys[i]=ty[i];}
 return 1;
}

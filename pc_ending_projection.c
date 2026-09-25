#include "pc_ending_projection.h"
/* Q20 quarter-wave table, rounded offline; no runtime floating point. */
static const int32_t sine[91]={
0,18300,36595,54878,73145,91389,109606,127789,145934,164033,
182083,200078,218011,235878,253673,271391,289027,306574,324028,341383,
358634,375776,392803,409711,426494,443147,459665,476044,492277,508360,
524288,540057,555661,571095,586356,601438,616338,631049,645568,659890,
674012,687928,701634,715127,728402,741455,754282,766880,779244,791370,
803256,814897,826289,837430,848316,858943,869309,879410,889243,898805,
908093,917105,925838,934288,942454,950333,957922,965219,972223,978930,
985339,991448,997255,1002758,1007956,1012847,1017429,1021701,1025662,1029311,
1032646,1035666,1038371,1040760,1042832,1044586,1046022,1047139,1047937,1048416,
1048576
};
static int32_t sin_degree(int angle) {
    angle%=360;if(angle<0)angle+=360;
    if(angle<=90)return sine[angle];
    if(angle<=180)return sine[180-angle];
    if(angle<=270)return -sine[angle-180];
    return -sine[360-angle];
}
int pc_ending_project_wide(const PcEndingView *v,int16_t x,int16_t y,int16_t z,
                           int32_t *sx,int32_t *sy) {
    if(!v || !sx || !sy)return 0;
    /* Bound intermediate products, including centre*depth, on 68000. */
    if(x< -8192 || x>8192 || y< -8192 || y>8192 || z< -8192 || z>8192 ||
       v->depth< -8192 || v->depth>8192 || v->tilt< -90 || v->tilt>90 ||
       v->focal_x>4096 || v->focal_y>4096)return 0;
    long long a=sin_degree(-v->tilt),b=sin_degree(90-v->tilt);
    long long rx=(long long)x*1048576;
    long long ry=(long long)y*b-(long long)z*a;
    long long rz=(long long)y*a+(long long)z*b;
    a=sin_degree(v->otis);b=sin_degree(90+v->otis);
    long long xx=(rx*b+rz*a)/1048576;
    rz=(rz*b-rx*a)/1048576;
    rz+=(long long)(v->depth-5*v->tilt)*1048576;
    if(rz<0)rz=-rz;
    if(!rz)rz=104858; /* PC exactly-zero depth fallback: 0.1 */
    long long px=(xx*v->focal_x+(long long)v->centre_x*rz)/rz;
    long long py=(ry*v->focal_y+(long long)v->centre_y*rz)/rz;
    if(px< -1000000000 || px>1000000000 || py< -1000000000 || py>1000000000)return 0;
    *sx=(int32_t)px;*sy=(int32_t)py;
    return 1;
}

int pc_ending_project(const PcEndingView *v,int16_t x,int16_t y,int16_t z,
                      int16_t *sx,int16_t *sy) {
    int32_t px,py;
    if(!sx || !sy || !pc_ending_project_wide(v,x,y,z,&px,&py))return 0;
    *sx=px>32767 ? 32767:px< -32767 ? -32767:(int16_t)px;
    *sy=py>32767 ? 32767:py< -32767 ? -32767:(int16_t)py;
    return 1;
}

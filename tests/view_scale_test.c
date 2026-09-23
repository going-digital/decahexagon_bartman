#include <assert.h>
#include <math.h>
#include <stdio.h>
#include "../view_scale.h"

int main(void) {
    /* Independent constants measured from native PC initfov(), mode 0. */
    const double pc_x=665.107486138/850.0*320.0/768.0;
    const double pc_y=707.017178437/850.0*200.0/480.0;
    double x_scale=view_scale_x(16384)/32768.0;
    double y_scale=view_scale_y(16384)/32768.0;
    assert(fabs(x_scale/pc_x-1)<0.0001);
    assert(fabs(y_scale/pc_y-1)<0.0001);
    double worst=0;
    for (unsigned angle=0;angle<1024;++angle) {
        double a=angle*6.283185307179586/1024;
        int16_t x=view_scale_x((int16_t)floor(cos(a)*16384));
        int16_t y=view_scale_y((int16_t)floor(sin(a)*16384));
        for (unsigned radius=0;radius<=600;++radius) {
            /* Includes gameplay radius truncation and signed pixel rounding. */
            int px=((int32_t)x*(int32_t)(radius/2))>>14;
            int py=((int32_t)y*(int32_t)(radius/2))>>14;
            double ex=fabs(px-cos(a)*radius*pc_x);
            double ey=fabs(py-sin(a)*radius*pc_y);
            assert(ex<1.5 && ey<1.5);
            if(ex>worst)worst=ex;
            if(ey>worst)worst=ey;
        }
    }
    printf("Baseline view: half-extents %.3f x %.3f world units; "
           "scale/rounding error below %.3f pixels (excluding trig approximation)\n",
           160/x_scale,100/y_scale,worst);
}

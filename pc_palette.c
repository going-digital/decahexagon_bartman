#include "pc_palette.h"

/* Extracted by executing graphicsclass::setpal in the owned PC binary.
 * See tests/fixtures/pc_palette_native.txt.gz and tests/PALETTE_REFERENCE.md.
 * Each endpoint is background RGB followed by primary-wall RGB. */
static const struct { uint16_t id; uint8_t start[6], end[6]; } schemes[] = {
    {0, {71,17,5,193,17,30}, {61,51,6,215,189,17}},
    {1, {68,0,67,186,15,181}, {71,9,5,201,71,57}},
    {3, {0,69,68,15,186,186}, {7,5,71,67,57,201}},
    {5, {0,0,0,255,128,0}, {0,0,0,0,255,128}},
    {6, {255,255,255,0,127,255}, {255,255,255,255,0,127}},
    {7, {0,127,255,215,235,255}, {255,0,127,255,215,235}},
    {9, {76,76,76,242,242,242}, {152,152,152,255,255,255}},
    {10, {11,82,0,70,249,16}, {11,82,0,70,249,16}},
    {11, {24,0,82,109,16,249}, {24,0,82,109,16,249}},
    {12, {82,11,0,249,70,16}, {82,11,0,249,70,16}},
    {13, {0,78,82,16,215,249}, {0,78,82,16,215,249}},
    {14, {82,0,78,249,16,215}, {82,0,78,249,16,215}},
    {15, {82,76,0,242,249,16}, {82,76,0,242,249,16}},
    {30, {0,0,0,255,255,255}, {0,0,0,255,255,255}},
    /* Ending ID 200 uses the PC setpal default yellow-to-red endpoints. */
    {200, {32,32,0,255,255,0}, {32,0,0,255,0,0}},
};

static void set_scheme(PcPalette *p, uint16_t id) {
    for (unsigned i=0; i<sizeof(schemes)/sizeof(schemes[0]); ++i) {
        if (schemes[i].id != id) continue;
        p->scheme=id;
        for (unsigned c=0;c<6;++c) {
            p->start[c]=schemes[i].start[c];p->end[c]=schemes[i].end[c];
        }
        return;
    }
}
static void interpolate(PcPalette *p) {
    for (unsigned i=0;i<6;++i)
        p->rgb[i]=(uint8_t)(p->start[i]+
            ((int32_t)p->end[i]-p->start[i])*p->blend/255);
}
void pc_palette_reset(PcPalette *p, uint8_t stage) {
    p->direction=p->change=0;p->target=0;p->blend=0;
    p->stage=stage;
    set_scheme(p,stage==2 ? 10 : stage==1 ? 5 : 0);
    interpolate(p);
}
void pc_palette_enter(PcPalette *p,uint8_t stage,uint8_t hyper) {
    p->stage=stage;
    set_scheme(p,hyper ? (stage==2 ? 9:stage==1 ? 6:3):
                         (stage==2 ? 10:stage==1 ? 5:0));
}
void pc_palette_start(PcPalette *p,uint8_t stage,uint8_t hyper) {
    pc_palette_reset(p,stage);pc_palette_enter(p,stage,hyper);interpolate(p);
}
void pc_palette_ending_start(PcPalette *p) {
    pc_palette_reset(p,4);set_scheme(p,30);interpolate(p);
}
void pc_palette_request(PcPalette *p,uint16_t id) {
    p->target=id;p->change=1;
}
void pc_palette_tick(PcPalette *p, uint32_t score) {
    /* Preserve PC ordering, including the idle endpoint tick and the extra
     * +4 during a pending scheme fade. A simple triangle wave differs here. */
    if (p->direction==2) {
        p->blend-=4;
        if (p->blend<=0) { p->blend=0;p->direction=0; }
    } else if (p->direction==1) {
        p->blend+=4;
        if (p->blend>=255) { p->blend=255;p->direction=2; }
    } else p->direction=1;
    interpolate(p);
    if (p->change==1) {
        p->blend-=25;
        if (p->blend<=0) {
            p->blend=0;
            set_scheme(p,p->target);
            /* PC setpal(1000): retain the displayed RGB as the new start. */
            for (unsigned c=0;c<6;++c) p->start[c]=p->rgb[c];
            p->scheme=1000;p->change=2;
        }
    } else if (p->change==2) {
        p->blend+=4;
        if (p->blend>=255) {
            p->blend=255;p->direction=2;p->change=0;
            set_scheme(p,p->target);
        }
    }
    if (p->stage==0) {
        if (score>7200 && score<=10800 && p->scheme==3 && !p->change) pc_palette_request(p,1);
        else if (score>3600 && score<=7200 && p->scheme==0 && !p->change) pc_palette_request(p,3);
    } else if (p->stage==1) {
        if (score>7200 && score<=10800 && p->scheme==6 && !p->change) pc_palette_request(p,7);
        else if (score>3600 && score<=7200 && p->scheme==5 && !p->change) pc_palette_request(p,6);
    } else {
        if (score>7200) {
            if (p->scheme!=30 && !p->change) pc_palette_request(p,30);
        } else if (score>3600) {
            if (p->scheme>9 && !p->change) pc_palette_request(p,9);
        } else if (score && score%120==0) {
            pc_palette_request(p,(uint16_t)(p->scheme+1-((p->scheme-9)/6)*6));
        }
    }
}
uint16_t pc_palette_colour(const PcPalette *p,uint8_t foreground) {
    const uint8_t *rgb=p->rgb+(foreground ? 3:0);
    /* Nearest of the 16 OCS channel levels (0,17,...255), no dithering. */
    return (uint16_t)((((rgb[0]+8)/17)<<8)|(((rgb[1]+8)/17)<<4)|((rgb[2]+8)/17));
}

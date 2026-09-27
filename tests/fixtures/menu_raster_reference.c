/* Reference pixel-at-a-time menu renderer for differential tests. */
static const UBYTE front_font[26][7]={
 {14,17,17,31,17,17,17},{30,17,17,30,17,17,30},{14,17,16,16,16,17,14},
 {30,17,17,17,17,17,30},{31,16,16,30,16,16,31},{31,16,16,30,16,16,16},
 {14,17,16,23,17,17,15},{17,17,17,31,17,17,17},{14,4,4,4,4,4,14},
 {7,2,2,2,18,18,12},{17,18,20,24,20,18,17},{16,16,16,16,16,16,31},
 {17,27,21,21,17,17,17},{17,25,21,19,17,17,17},{14,17,17,17,17,17,14},
 {30,17,17,30,16,16,16},{14,17,17,17,21,18,13},{30,17,17,30,20,18,17},
 {15,16,16,14,1,1,30},{31,4,4,4,4,4,4},{17,17,17,17,17,17,14},
 {17,17,17,17,17,10,4},{17,17,17,21,21,21,10},{17,17,10,4,10,17,17},
 {17,17,10,4,4,4,4},{31,1,2,4,8,16,31}
};
static void front_text(UBYTE *plane,const char *text,unsigned y,unsigned scale) {
    unsigned len=0;while(text[len])++len;
    if(scale==2) {
        unsigned left=(SCREEN_WIDTH-len*16)/2;
        for(unsigned c=0;c<len;++c) {
            unsigned ch=(unsigned char)text[c];
            if(ch<'A' || ch>'Z')continue;
            for(unsigned row=0;row<16;++row) {
                UWORD bits=menu_font16[ch-'A'][row];
                for(unsigned x=0;x<16;++x)if(bits&(0x8000u>>x)) {
                    unsigned px=left+c*16+x;
                    plane[(y+row)*SCREEN_WIDTH_BYTES+px/8]|=128>>(px&7);
                }
            }
        }
        return;
    }
    unsigned left=(SCREEN_WIDTH-len*6*scale)/2;
    for(unsigned c=0;c<len;++c)for(unsigned row=0;row<7;++row) {
        unsigned ch=(unsigned char)text[c],bits=0;
        if(ch>='A' && ch<='Z')bits=front_font[ch-'A'][row];
        else if(ch>='0' && ch<='9') {
            static const UBYTE digits[10][7]={{14,17,19,21,25,17,14},{4,12,4,4,4,4,14},{14,17,1,2,4,8,31},{30,1,1,14,1,1,30},{2,6,10,18,31,2,2},{31,16,16,30,1,1,30},{14,16,16,30,17,17,14},{31,1,2,4,8,8,8},{14,17,17,14,17,17,14},{14,17,17,15,1,1,14}};
            bits=digits[ch-'0'][row];
        } else if(ch=='.')bits=row==6?4:0;
        else if(ch==':')bits=(row==2 || row==5)?4:0;
        else if(ch=='/')bits=1u<<(row<5?row:4);
        else if(ch=='-')bits=row==3?14:0;
        else if(ch=='_')bits=row==6?31:0;
        for(unsigned x=0;x<5;++x)if(bits&(16>>x))
            for(unsigned dy=0;dy<scale;++dy)for(unsigned dx=0;dx<scale;++dx) {
                unsigned px=left+(c*6+x)*scale+dx,py=y+row*scale+dy;
                plane[py*SCREEN_WIDTH_BYTES+px/8]|=128>>(px&7);
            }
    }
}
void hud_draw_front(void *buffer) {
    UBYTE *plane=buffer;
    /* Draw only into the unpublished buffer, after its blits have finished. */
    for(unsigned y=24;y<184;++y)for(unsigned x=0;x<SCREEN_WIDTH_BYTES;++x)
        plane[y*SCREEN_WIDTH_BYTES+x]=0;
    unsigned page=game_front_page();
    if(page==0) {
        static const char *items[]={"START","OPTIONS","CREDITS"};
        front_text(plane,"HEXAGON",32,2);
        for(unsigned i=0;i<3;++i) {
            unsigned y=70+i*26;front_text(plane,items[i],y,2);
            if(i==game_front_choice())for(unsigned x=88;x<232;++x)
                plane[(y+17)*SCREEN_WIDTH_BYTES+x/8]|=128>>(x&7);
        }
        front_text(plane,"LEFT / RIGHT TO CHOOSE",158,1);
        front_text(plane,"SPACE / RETURN / FIRE TO SELECT",173,1);
    } else if(page==1) {
        front_text(plane,"OPTIONS",36,2);
        front_text(plane,"ARCADE MODE: OFF",94,1);
        front_text(plane,"SPACE / RETURN / FIRE TO CHANGE",120,1);
        front_text(plane,"ESC TO RETURN",173,1);
    } else {
        static const char *pages[5][7]={
            {"ORIGINAL GAME CONCEPT AND DESIGN","TERRY CAVANAGH","WWW.DISTRACTIONWARE.COM","ORIGINAL SOUNDTRACK","CHIPZEL","CHIPZELMUSIC.BANDCAMP.COM",""},
            {"VOICE","JENN FRANK","WWW.INFINITELIVES.NET","PC PORT","ETHAN LEE","FLIBITIJIBIBO.COM",""},
            {"AMIGA PORT","GOING DIGITAL","ADDITIONAL CODE","A/B - KEIR FRASER","EMMANUEL MARTY","ASTRA - SONNET",""},
            {"TESTING AND FEEDBACK","ALISTAIR ROBINSON - AMBROID","FRIAR - JANK FACTOR - SEIFER - JC","NAG_GRAHAM - PROMETHEUS - RETRO32 - ZENDAR","ADDITIONAL ASSISTANCE","NAG - NORWICH GAMEDEVS","SPAG - AMIGAGAMEDEV"},
            {"PLAY SUPER HEXAGON","STEAM - GOG - ANDROID - IOS","FONT - BUMP IT UP BY AARON AMAR","GET THE CHIPZEL SOUNDTRACK","CHIPZELMUSIC.BANDCAMP.COM","","THANK YOU FOR PLAYING"}
        };
        front_text(plane,"CREDITS",30,2);
        for(unsigned i=0;i<7;++i)front_text(plane,pages[game_credit_page()][i],56+i*13,1);
        char counter[]="PAGE 1 / 5";counter[5]+=game_credit_page();front_text(plane,counter,153,1);
        front_text(plane,"LEFT / RIGHT OR FIRE - ESC TO RETURN",173,1);
    }
}


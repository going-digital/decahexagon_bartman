#include <assert.h>
#include <string.h>
#include "../menu_font16.h"
#include "../menu_carousel.h"
#include "../front_menu.h"
int main(void) {
 unsigned char strip[MENU_STRIP_SIZE],plane[8032];
 const char *items[]={"START","OPTIONS","CREDITS"};
 menu_strip_init(strip);
 for(unsigned p=0;p<576;++p) {
  memset(plane,0xa5,sizeof(plane));menu_strip_window(strip,plane+16,p);
  for(unsigned y=0;y<16;++y)for(unsigned x=0;x<320;++x) {
   unsigned wx=(x+p)%576,expected=0;
   for(int n=-1;n<4;++n) {
    const char *s=items[(n+3)%3];int len=(int)strlen(s);
    int local=(int)wx-(160+n*192-len*8);
    if(local>=0 && local<len*16)
     expected|=(menu_font16[s[local/16]-'A'][y]>>(15-local%16))&1;
   }
   assert(((plane[16+(92+y)*40+x/8]>>(7-x%8))&1)==expected);
  }
  for(unsigned i=0;i<sizeof(plane);++i)
   if(i<16+92*40 || i>=16+108*40)assert(plane[i]==0xa5);
 }
 for(unsigned locks=0;locks<8;++locks) {
 menu_strip_init_for(strip,1|(locks<<11));
 /* Six-level strip repeats the three base names, with NORMAL/HYPER in the HUD. */
 const char *levels[]={"HEXAGON","HEXAGONER","HEXAGONEST"};
 for(unsigned p=0;p<1152;++p) {
  memset(plane,0xa5,sizeof(plane));menu_strip_window(strip,plane+16,p);
  for(unsigned y=0;y<16;++y)for(unsigned x=0;x<320;++x) {
   unsigned wx=(x+p)%1152,expected=0;
   for(int n=-1;n<7;++n) {
    unsigned profile=(n+6)%6;
    const char *s=(profile>=3 && (locks&(1u<<(profile-3))))?"LOCKED":levels[profile%3];int len=(int)strlen(s);
    int local=(int)wx-(160+n*192-len*8);
    if(local>=0 && local<len*16)expected|=(menu_font16[s[local/16]-'A'][y]>>(15-local%16))&1;
   }
   assert(((plane[16+(92+y)*40+x/8]>>(7-x%8))&1)==expected);
  }
 }
 }
 struct FrontMenu level={0};level.page=FRONT_LEVELS;
 assert(front_menu_tick(&level,1,0,0)==SFX_BIT(SFX_MENUCHOOSE));
 assert(level.level==5 && level.slide==-192);
 for(unsigned i=0;i<32;++i)front_menu_tick(&level,0,0,0);
 assert(front_menu_tick(&level,2,0,0)==SFX_BIT(SFX_MENUCHOOSE));
 assert(level.level==0 && level.slide==192);
 assert(front_menu_tick(&level,0,0,1)==SFX_BIT(SFX_RANKUP));
 assert(level.page==FRONT_HOME && level.slide==0);
 for(unsigned choice=0;choice<3;++choice)for(unsigned direction=1;direction<=2;++direction) {
  struct FrontMenu m={0};m.choice=choice;
  assert(front_menu_tick(&m,direction,0,0)==SFX_BIT(SFX_MENUCHOOSE));
  assert(m.slide==(direction==2?192:-192));
  int last=192;
  for(unsigned i=0;i<32;++i) {
   assert(front_menu_tick(&m,direction,0,0)==0);
   int distance=m.slide<0?-m.slide:m.slide;assert(distance<=last);last=distance;
  }
  assert(!m.slide);
 }
 return 0;
}

#include <assert.h>
#include "../front_menu.h"
int main(void) {
    struct FrontMenu m={0};
    assert(m.page==FRONT_HOME);
    assert(front_menu_tick(&m,0,0,1)==0);
    assert(front_menu_tick(&m,0,1,0)==SFX_BIT(SFX_MENUSELECT));assert(m.page==FRONT_LEVELS);
    assert(front_menu_tick(&m,0,0,1)==SFX_BIT(SFX_RANKUP));assert(m.page==FRONT_HOME);
    assert(front_menu_tick(&m,2,0,0)==SFX_BIT(SFX_MENUCHOOSE));assert(m.choice==1);
    assert(front_menu_tick(&m,2,0,0)==0);assert(m.choice==1); /* held is not repeated */
    assert(front_menu_tick(&m,0,1,0)==SFX_BIT(SFX_MENUSELECT));assert(m.page==FRONT_OPTIONS);
    assert(front_menu_tick(&m,0,1,0)==0);assert(m.page==FRONT_OPTIONS);
    assert(front_menu_tick(&m,0,0,1)==SFX_BIT(SFX_RANKUP));assert(m.page==FRONT_HOME);
    front_menu_tick(&m,2,0,0);assert(m.choice==2);
    front_menu_tick(&m,0,1,0);assert(m.page==FRONT_CREDITS && !m.credits);
    for(unsigned i=0;i<FRONT_CREDIT_PAGES;++i)front_menu_tick(&m,0,1,0);
    assert(!m.credits);
    assert(front_menu_tick(&m,1,0,0)==SFX_BIT(SFX_MENUCHOOSE));assert(m.credits==FRONT_CREDIT_PAGES-1);
    assert(front_menu_tick(&m,0,0,1)==SFX_BIT(SFX_RANKUP));assert(m.page==FRONT_HOME && m.choice==2);
    front_menu_tick(&m,2,0,0);assert(m.choice==0);
    assert(front_menu_tick(&m,0,1,0)==SFX_BIT(SFX_MENUSELECT));assert(m.page==FRONT_LEVELS);
    return 0;
}

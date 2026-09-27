#include <assert.h>
#include <string.h>
#include "arcade.h"
int main(void) {
    Arcade a={0};
    assert(ARCADE_ROWS==5);
    for(unsigned p=0;p<6;++p) {
        assert(arcade_rank(&a,p,0)==-1);
        for(unsigned n=1;n<=8;++n) {
            assert(arcade_enter(&a,p,n*60));assert(a.row==0 && a.pending);
            arcade_type(&a,'A'+n);arcade_finish(&a);
        }
        for(unsigned r=0;r<5;++r)assert(a.scores[p][r].ticks==(8-r)*60);
        assert(arcade_rank(&a,p,4*60)==-1);
        assert(arcade_enter(&a,p,7*60));assert(a.row==2);
        assert(a.scores[p][1].name[0]=='H'); /* existing tie keeps its name */
        for(unsigned i=0;i<20;++i)arcade_type(&a,'Z');
        assert(a.length==10 && strlen(a.scores[p][2].name)==10);
        arcade_type(&a,8);arcade_type(&a,'9');
        assert(!strcmp(a.scores[p][2].name,"ZZZZZZZZZ9"));
        for(unsigned i=0;i<11;++i)arcade_type(&a,8);
        arcade_finish(&a);assert(!strcmp(a.scores[p][2].name,"ANON"));
        assert(!a.pending);arcade_type(&a,'X');assert(!strcmp(a.scores[p][2].name,"ANON"));
    }
    assert(arcade_rank(&a,6,123)==-1);
    assert(arcade_key(0x10)=='Q' && arcade_key(0x28)=='L');
    assert(arcade_key(0x31)=='Z' && arcade_key(0x37)=='M');
    assert(arcade_key(10)=='0' && arcade_key(0x40)==' ');
    assert(arcade_key(0x41)==8 && !arcade_key(0x44) && !arcade_key(0x45));
    return 0;
}

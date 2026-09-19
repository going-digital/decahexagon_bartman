#include <assert.h>
#include "../config.h"
int main(void) {
    for(unsigned pal=0;pal<2;++pal) {
        video_select(pal);
        assert(video_timing.lines==(pal?312:262));
        assert(DISPLAY_RATE==(pal?50:60));
        assert(DISPLAY_HW_Y==(pal?72:44));
        assert(video_timing.music_period==(pal?296:298));
        assert(video_timing.sfx_period==(pal?443:447));
        assert(DISPLAY_HW_Y+SCREEN_HEIGHT<video_timing.lines);
    }
    return 0;
}

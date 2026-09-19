#include <assert.h>
#include "../config.h"
int main(void) {
    const unsigned fields[]={312,313,262,263};
    for(unsigned i=0;i<4;++i) {
        video_select(fields[i]);
        assert(video_timing.lines==fields[i]);
        assert(DISPLAY_RATE==(i<2?50:60));
        assert(DISPLAY_HW_Y==(i<2?72:44));
        assert(video_timing.music_period==(i<2?296:298));
        assert(video_timing.sfx_period==(i<2?443:447));
        assert(DISPLAY_HW_Y+SCREEN_HEIGHT<video_timing.lines);
    }
    return 0;
}

#pragma once
/* Called once per nominal 60 Hz simulation tick. */
typedef struct { unsigned active, was_playing, fade_in, fade_out, volume; } PcmLifecycle;
enum { PCM_START=1, PCM_STOP=2 };
unsigned pcm_lifecycle_tick(PcmLifecycle *s,unsigned playing,unsigned menu);

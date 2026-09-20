#pragma once
/* Called once per nominal 60 Hz simulation tick. */
typedef struct { unsigned active, was_playing, fade_in, fade_out, volume; } PcmLifecycle;
enum { PCM_START=1, PCM_STOP=2 };
unsigned pcm_lifecycle_tick(PcmLifecycle *s,unsigned playing,unsigned menu);
/* PC track 1 (Courtesy): first start is zero, subsequent starts pick a cue. */
unsigned pcm_start_offset_ms(unsigned previously_started,unsigned random_value);

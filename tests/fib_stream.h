#pragma once
void fib_stream_start(void);
void fib_stream_fill(void);
void fib_stream_draw(unsigned char *plane);
void fib_stream_stop(void);
void fib_stream_frame(unsigned elapsed);
void fib_stream_tick(unsigned playing,unsigned menu);

unsigned fib_stream_cue(void);

#include "../fib_pcm.h"
/* Caller owns a validated, gain-adjusted bank, cue bytes and 2048-byte even
 * Chip DMA storage. Bind/unbind only after stop; memory must remain live until
 * unbound. External tracks start at zero until their retry offsets are supplied.
 * Null cues are allowed (no visual cue), never substituted with Courtesy. */
int fib_stream_bind(const PcmSong *source, unsigned samples,
                    const unsigned char *cues, unsigned cue_bytes,
                    unsigned char *chip_buffers);
int fib_stream_unbind(void);

/* Counters for target playback verification; each is an atomic 16-bit snapshot. */
void fib_stream_status(unsigned *underrun_count,unsigned *completed_blocks);

/* Set after successful bind, before starting. Bind defaults to Courtesy 612.
 * Use 0 for PC Otis, 582 for the verified Focus source. Stops do not reset it. */
int fib_stream_set_cue_lead(unsigned samples);

/* Bind the shipped Focus bank first. Plays the normal-ending stretch once,
 * then silence; menu/retry cancels. Secret ending must not call this. */
int fib_stream_start_ending_focus(void);
/* Same producer for the playable Hexagonest bonus, stopped on death/menu. */
int fib_stream_start_bonus_focus(void);
/* Unmixed grain segments are copied in the main loop, never VBlank. */
void fib_stream_fill_main(void);

/* Secret ending: bound Focus backwards, original sample rate, once then silence. */
int fib_stream_start_reverse_focus(void);

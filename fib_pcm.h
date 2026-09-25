#pragma once
/* Predecoded FBP1/FBP2 transport state; no codec cache or interpolation state. */
typedef struct {
    const unsigned char *sequence, *offsets, *bank, *edges, *raw;
    unsigned pcm_split;
    unsigned short seq_count, seq_index, output_left;
} PcmSong;
int fib_song_init(PcmSong *s,const unsigned char *blob,unsigned bytes);
void fib_song_read(PcmSong *s,unsigned char *out,unsigned samples);
/* Seek within the logical PCM stream; physical slice padding is skipped. */
int fib_song_seek(PcmSong *s,unsigned sample);
int fib_pcm_init_split(PcmSong *s,const unsigned char *first,unsigned first_bytes,
                       const unsigned char *second,unsigned second_bytes);

/* 512-sample grains / 256-sample hops, Q8 linear crossfade. Schedule positions
 * refer to the logical decoded bank, not physical dictionary addresses. */
typedef struct {
    PcmSong source;
    const unsigned *offsets;
    unsigned count, total, position;
    signed char previous[256], grain[512], output[256];
} PcmStretch;
int fib_stretch_init(PcmStretch *s,const PcmSong *source,unsigned source_samples,
                     const unsigned *offsets,unsigned count,unsigned total);
void fib_stretch_read(PcmStretch *s,unsigned char *out,unsigned samples);

/* Unmixed 256-sample hop: old tail on channel A, new head on channel B.
 * The first hop uses A only. Output tail is zero padded. */
void fib_stretch_channels(PcmStretch *s,unsigned char *a,unsigned char *b);

/* Read backwards through the logical song; zero-pad once remaining reaches 0.
 * Reverses copied bytes only, leaving the cached bank unchanged. */
void fib_song_read_reverse(PcmSong *s,unsigned *remaining,unsigned char *out,unsigned samples);

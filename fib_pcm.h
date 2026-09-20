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

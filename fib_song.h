#pragma once
/* Legacy host/archived FBS1/FBS2 decoder; not linked into the game.
 * The predecoded target uses the compact state in fib_pcm.h instead. */
typedef struct {
    const unsigned char *blob, *sequence, *offsets, *bank, *edges, *raw, *packed;
    unsigned short seq_count, bank_count, seq_index, left, block_left;
    unsigned short output_left, source_length, target_length, source_index, phase;
    unsigned short low_pending, codes, predictor, raw_mode, aligned;
    int a,b;
    unsigned pcm_split;
    unsigned short cache_pos,cache_count;
    unsigned char cache[512];
} FibSong;
int fib_song_init(FibSong *s,const unsigned char *blob,unsigned bytes);
void fib_song_read(FibSong *s,unsigned char *out,unsigned samples);
int fib_pcm_init_split(FibSong *s,const unsigned char *first,unsigned first_bytes,
                       const unsigned char *second,unsigned second_bytes);

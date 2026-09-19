#pragma once
/* FBS1 metadata is big-endian; enclosed FIB1 headers remain little-endian.
 * No allocations: a 512-byte decode cache plus two interpolation samples. */
typedef struct {
    const unsigned char *blob, *sequence, *offsets, *bank, *edges, *raw, *packed;
    unsigned short seq_count, bank_count, seq_index, left, block_left;
    unsigned short output_left, source_length, target_length, source_index, phase;
    unsigned short low_pending, codes, predictor, raw_mode;
    int a,b;
    unsigned short cache_pos,cache_count;
    unsigned char cache[512];
} FibSong;
int fib_song_init(FibSong *s,const unsigned char *blob,unsigned bytes);
void fib_song_read(FibSong *s,unsigned char *out,unsigned samples);

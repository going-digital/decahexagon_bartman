#pragma once
/* FBS1/FBS2 metadata is big-endian; enclosed FIB1 headers remain little-endian.
 * FBP1 uses the same API with an offline-decoded, word-padded dictionary.
 * No allocations: shared state includes a 512-byte cache (unused by PCM). */
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

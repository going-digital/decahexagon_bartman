#pragma once
/* Decode one independently seeded FIB1 payload block (no file header).
 * Caller validates 1 <= samples <= 512 and provides
 * 1 + ceil((samples-1)/2) source bytes and samples destination bytes.
 * Unsigned predictor arithmetic deliberately wraps modulo 256.
 */
void fib_decode_block(const unsigned char *src, unsigned char *dst, unsigned samples);

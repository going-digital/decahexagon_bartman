#pragma once
/* Expand FIB1 at arena[0] to PCM at arena[0]. Validates before writing.
 * capacity includes the original input and entire output; scratch is 257 bytes.
 * Loading-time only. Returns decoded length, or zero for malformed input. */
unsigned trackloader_fib_expand(unsigned char *arena, unsigned input_bytes,
                               unsigned capacity);

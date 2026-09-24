#pragma once
#include <stdint.h>
/* Exclusive caller-owned Chip memory; init only while all clients are stopped.
 * Eight-byte alignment, cleared allocations, arbitrary-order frees/coalescing.
 * Main-thread only. No OS calls, allocation or interrupt locking. */
int track_chip_init(void *memory, uint32_t bytes);
void *track_chip_alloc(uint32_t bytes);
void track_chip_free(void *memory);
uint32_t track_chip_available(void);

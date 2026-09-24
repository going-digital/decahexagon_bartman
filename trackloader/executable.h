#pragma once
#include <stdint.h>
typedef struct { uint32_t entry, memory_bytes; } TrackExecutable;
/* EXE1 -> relocated image at the beginning of this same arena; clears BSS.
 * base must be the arena's target address, even on host simulations. Entire
 * package, checksum and fixups are validated before any arena/output writes.
 * Caller must verify trusted expected package length/hash before inflate, own
 * max(package bytes,memory bytes), and synchronize instruction caches if needed.
 * No execution, allocation or OS calls. Info must not overlap the arena. */
int track_executable_prepare(unsigned char *arena,uint32_t bytes,
    uint32_t capacity,uint32_t base,TrackExecutable *info);

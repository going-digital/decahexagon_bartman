# Release memory use

**Baseline visibility update:** separate camera-scaled X/Y direction tables
add 2,048 bytes of ordinary BSS. Scaling runs during table initialization;
the per-vertex path retains two 68000 multiplies and drops the old Y squeeze.

**Display pipeline update:** three 1,024-byte copper lists now replace the
original single list, adding 2,048 bytes of runtime Chip RAM to the historical
figures below (1,024 bytes beyond the first handoff fix). An 8-byte fixed
dispatcher replaces the old 4-byte parked list. The third list pairs with the
existing third bitplane/sprite buffers: it can be rendered while another frame
awaits presentation. The old display is acknowledged before reuse. Bitplanes
are cleared at allocation; no extra bitplane or player sprite bank is needed.

**Packed-disk update (execram 1.3.0):** memory classes are preserved and the
music bank is embedded again. The packed executable allocates 222,904 ordinary
bytes and 242,604 Chip bytes, plus 31,672 bytes of runtime Chip allocations.
Accounted runtime RAM is **497,180 bytes**, including **274,276 mandatory Chip
bytes**. The startup depacker uses an additional 1,580-byte scratch hunk, freed
before the game starts. OS/stack/loader overhead remains excluded. The packed
file is 256,500 bytes; there is no sidecar bank. The breakdown below records
the preceding unpacked release and its cleanup savings.

Measured for the universal music + SFX build, `BUILD_DEBUG=0`, `CHEAT_MODE=0`,
Courtesy 12 kHz / 96 slices. Executable sizes come from the actual HUNK file;
resident sections come from its allocation table, not ELF debug-file size.

| Allocation | Before | After | Saved |
| --- | ---: | ---: | ---: |
| Executable file | 477,228 | 465,072 | 12,156 |
| Resident hunks (all memory classes) | 478,100 | 465,504 | 12,596 |
| Runtime allocations | 32,512 | 31,672 | 840 |
| **Accounted runtime RAM** | **510,612** | **497,176** | **13,436** |
| Of that, mandatory Chip RAM | 285,224 | 274,272 | 10,952 |

This excludes AmigaOS, process stack, allocator/loader overhead and temporary
loading space. Ordinary hunks can also occupy Chip RAM if other RAM is scarce.
These figures do **not** establish compatibility with a 512 KB-only machine;
the combined build still targets 512 KB Chip plus 512 KB expansion.

## Removed

- **10,112 Chip bytes:** SFX bank 117,890 → 107,778. Omit the 2,756-byte
  `menuselect` payload (its PC screens do not exist), preserving its ID as an
  empty guarded entry. The other 7,356 bytes were exact trailing digital zeros.
  The original Ogg assets remain available for future work.
- **840 Chip bytes:** allocate only timer glyphs that can actually be selected:
  ten digits in each of five slots, and one decimal-point sprite. Previously
  every slot cached all twelve glyphs: 72 sprites → 51, each 40 bytes.
- **556 bytes of BSS:** replace the 586-byte legacy codec/interpolation state
  with a 30-byte PCM-only state. Archived compression experiments remain host
  tools; their cache is no longer resident in the game.
- **1,928 bytes of code/read-only data:** net reduction after removing the
  release audio diagnostic overlay and the above changes. Set
  `-DAUDIO_DIAGNOSTICS=1` to restore the overlay for trials.

## Current breakdown

| Resident/runtime use | Bytes |
| --- | ---: |
| Code hunk | 36,996 |
| Ordinary read-only hunk, including music prefix and cue table | 174,752 |
| Ordinary data | 8 |
| BSS: walls, projected spans, sine table and game/audio state | 11,148 |
| Static Chip hunks: music tail, SFX and blank DMA data | 242,600 |
| Three 320×200 one-bit framebuffers | 24,000 |
| Copper allocation | 1,024 |
| Timer and banner sprite allocations | 4,600 |
| Four PCM DMA buffers | 2,048 |
| **Total** | **497,176** |

Music remains 272,412 bytes across the ordinary and Chip hunks. Effects are
107,778 bytes. Together they account for about 76% of accounted runtime RAM.
The original cue table is 11,441 bytes of the ordinary read-only hunk.

## Remaining options

- Render banner/glyph sprites on demand: modest further Chip savings, at the
  cost of updates and DMA-safe buffering. Current caches avoid that CPU work.
- Reduce the 500-wall capacity and matching projection array: potentially useful,
  but only after proving worst-case occupancy for every profile and transition.
- Two framebuffers instead of three could save 8,000 Chip bytes, but would
  change the asynchronous clearing/render pipeline and needs performance tests.
- Music and speech dominate. Larger savings require changing their quality,
  coverage or loading strategy; this pass preserves accepted music quality.

Unused functions already benefit from LTO and section garbage collection.
Deleting archived source files or ELF debug information does not itself reduce
the loaded game's memory.

Validation: host timing/SFX/lifecycle checks pass. `python3 tools/audio/check_pcm_memory.py` verifies 2,324,096 output samples (a full song
plus loop boundary) against the accepted PCM dictionary, with state/output
canaries. Every retained SFX sample matches its original conversion; omitted
suffixes contain only zeros, and all sample addresses/lengths remain even.

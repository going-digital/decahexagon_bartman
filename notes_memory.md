# Memory budget

Target min spec: **A500, OCS, 1 MB (512 KB chip + 512 KB trapdoor/slow)**.
Also must fit an unexpanded **512 KB chip-only** A500.

Chip RAM holds anything the custom chips DMA from: bitplanes, copper lists,
audio sample data, sprites. Everything else (code, tables, score/pattern
data, stack) can live in slow/fast RAM.

## Current footprint (measured from `out/a.elf`, debug build)

| Region | Bytes | Where | Notes |
|---|--:|---|---|
| `.text` (68k code) | 11,678 | any | grows with the game |
| `.rodata` + `.data` | 345 | any | |
| `.bss` | 2,152 | any | `sin_table` is 2,048 of it |
| `.INCBIN.MEMF_CHIP` | 198,055 | **chip** | LSP `technova_main.lsmusic` (7,921) + `.lsbank` (190,132), forced chip by `INCBIN_CHIP` |
| `.MEMF_CHIP` | 4 | chip | `copper2` end-of-list |
| bitplanes (AllocMem) | 24,000 | chip | 3 × `BITPLANE_SIZE` (200×40) |
| `copper1` (AllocMem) | 1,024 | chip | |

**Chip total now ≈ 223 KB** (193 KB of it is the music sample bank).
Unexpanded 512 KB A500: with code/bss also in chip, ≈ 237 KB used, ≈ 275 KB free.

## The problem: the LSP sample bank is 82 % of chip use

`technova_main.lsbank` (190 KB) is raw sample data and must be in chip for
Paula DMA. Every extra in-game track that needs its own instruments adds a
similar lump, and Phase 2 triples the bitplane cost.

Mitigations, roughly in order of preference:
1. **Crunch the bank on disk, depack to chip at startup** - the doynax
   depacker is already in the tree (`support/depacker_doynax*`). Typical
   ratio on sample data is modest but the `.lsmusic` score data crunches well.
2. **One shared instrument bank for all level tracks.** LSP supports several
   songs against a single bank - author the 6 tracks from one sample set.
3. Trim/downsample instruments; 8-bit, and drop the top octave of long samples.
4. Keep `.lsmusic` (score) out of chip - it's CPU-read only. Needs a non
   `INCBIN_CHIP` path or a copy-to-any-RAM at load.

## Projected additions

| Item | Est. chip | Phase |
|---|--:|---|
| Colour upgrade: 2 bitplanes × 3 buffers | 48,000 (was 24,000) | 2 |
| ...or 3 bitplanes × 3 buffers | 72,000 | 2 |
| Rotating wedge background buffer(s) | 8,000–16,000 | 2 |
| Announcer VO + SFX samples (crunched) | 20,000–48,000 | 4 |
| 2nd/3rd music bank if not shared | ~190,000 **each** | 4 |

## Working rule

Keep chip use **under ~256 KB** so the game runs on a 512 KB A500 with room
for stack + system. If the audio bank can't be shrunk under ~128 KB, the
512 KB-only target is off the table and 1 MB becomes the hard minimum.

## Build switches (see Makefile)

- `make EXTRA_CFLAGS="-DBUILD_DEBUG=0"` - release: no raster bar, no WinUAE
  hooks, no `KPrintF`/banner, no `warpmode`. ~1.8 KB smaller `.text`.
- `make EXTRA_CFLAGS="-DTARGET_NTSC"` - 60 Hz timing + pixel-aspect correction.

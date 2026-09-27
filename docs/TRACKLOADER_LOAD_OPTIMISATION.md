# Initial ADF load: contiguous OFS layout

The user reports roughly two minutes to start on an emulated A1200 with 2 MiB Chip, 8 MiB Fast and a 68040. That exact configuration has not been reproduced. Existing 68000 DEFLATE cycle measurements must not be presented as 68040 timings.

## Change

The ADF builder now repacks its OFS files in startup order: game, Courtesy, Otis, Focus, then the two legacy save fixtures. Each header/extension block is immediately followed by the data blocks it describes. The former allocator placed metadata such that the single-track cache repeatedly returned to earlier tracks.

This is an offline layout change, with no additional runtime RAM or CPU cost. Root sector 880, bitmap location, boot sectors, bootstrap tracks and persistent-save tracks remain fixed. All sector links, ownership fields, checksums and allocation bits are rebuilt. Save anchors are generated after repacking, so they describe the new root and bitmap. Payloads and compression remain identical; soundtrack preloading policy is unchanged.

## Measured read counts

The actual runtime C OFS reader was run on the earlier ADF and its repacked counterpart. Its read callbacks were grouped exactly as the native loader's single-track cache groups them. Returned bytes match for all six files. An independent inspector also checks references, ownership, bitmap and reserved sectors.

| Payload | Previous track reads | Repacked track reads |
| --- | ---: | ---: |
| Game | 27 | 22 |
| Courtesy | 57 | 46 |
| Otis | 56 | 45 |
| Focus | 57 | 46 |
| Total | 197 | 159 |

This is 38 fewer full-track reads (19.3%). Summed cylinder movement **within these individual loads** drops from 618 to 168 (72.8%). These counts exclude boot-stage and save reads, between-file initial seek positions, hardware retries, decompression and motor spin-up. They do not establish a percentage reduction in total boot time.

Only one repeat track remains within the three soundtrack loads, versus 34 previously. This greatly reduces the immediate benefit of adding a multi-track runtime cache; CPU/decode timing on the reported emulator is the next useful measurement.

## Verification

- Distribution builder runs independent OFS inspection before and after repacking, then the existing runtime-reader corruption checks.
- `python3 tests/trackloader_layout_test.py [optional-baseline.adf]` compares every payload through the actual C reader and verifies packing is idempotent. It was also run on the earlier, unoptimised release image.
- `make test-trackloader-layout` checks the currently built image.
- Updated distribution contains both ADF and WHDLoad. WHDLoad does not use this OFS disk layout and receives no loading-speed change from it.
- Hardware/A1200-68040 wall-clock timing remains to be confirmed by the tester.

Cold-boot validation passed in Copperline with A500 OCS/68000, 512 KiB Chip + 512 KiB Slow, both without Fast RAM and with 8 MiB Fast RAM. Both reached the main menu. Captures are `scratchpad/arcade/home.png` and `scratchpad/load_layout_fast/home.png`. These are compatibility checks, not a reproduction of the reported A1200/68040 timing. The ZIP's ADF matches the tested image byte for byte.

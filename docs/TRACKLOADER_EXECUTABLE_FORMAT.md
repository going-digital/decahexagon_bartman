# Resident executable package (EXE1)

This provisional format wraps the native 68000 game linked at address zero.
It is compressed as raw DEFLATE using the existing zultra library. It is not an
AmigaDOS executable and has no OS startup or hunk loader.

All fields and relocation words are unsigned big-endian 32-bit values:

| Offset | Field |
| --- | --- |
| 0 | Magic `EXE1` |
| 4 | Header bytes, exactly 32 |
| 8 | Initialized image bytes, including linker gaps |
| 12 | Resident image bytes, including trailing BSS |
| 16 | Entry offset, even and inside the initialized image |
| 20 | Relocation count |
| 24 | Standard reflected CRC-32 of the whole package, with this field zero |
| 28 | Reserved, must be zero |
| 32 | Sorted relocation offsets, four bytes each |
| after offsets | Initialized image |

Relocation offsets identify non-overlapping even-aligned four-byte addresses
within the initialized image. Each initially refers to an offset within the
resident image (one-past-end pointers allowed). Add the actual load base to each.
Hardware addresses and other absolute constants are not relocation entries.

`package_native_game.py` reads ELF32 big-endian m68k sections and retained RELA
records. It handles image-relative R_68K_32 fixups, leaves same-image PC-relative
references unchanged, and rejects unsupported kinds and interleaved BSS.
The linker must retain relocations with `--emit-relocs`; this is a build-time
ELF reader, not a parser for untrusted ELF files.

`track_executable_prepare` validates the header, lengths, capacity, checksum,
entry and every relocation before changing arena or output state. It then fixes
addresses, moves the image down over the package header/table and zeroes BSS.
The output descriptor returns the absolute entry and resident size. The caller
owns at least max(package bytes, resident bytes), plus any proven inflate tail.
The base argument must equal the actual destination on target. The function does
not execute code or perform cache maintenance. CRC detects accidental damage;
it does not authenticate executable content.

For the current package, 184,012 bytes cover all stages: its 169,361-byte package
inflates from an 84,030-byte stream at offset 85,334, requiring 169,364 bytes for
the measured overlap replay. These offsets belong to this exact payload hash;
regeneration requires measuring them again. Inflate is still the historical
unchecked core, so immutable disk identity/length/checksum checks remain a caller
requirement before entering it.

Reproduce after building the existing zultra library and Musashi inflate probe:

```
python3 tools/trackloader/check_native_game.py
python3 tools/trackloader/package_native_game.py
python3 tools/trackloader/check_executable.py
python3 tools/trackloader/check_executable_target.py --execram-source /path/to/execram
```

Host tests compare every relocated image byte and BSS against independent links
at 0x20000 and 0x40000, and reject 19 malformed cases without writes. Musashi runs
the actual preparer on a 68000 at both bases, including checksum failure. A
separate actual-68000 inflate run measures and replays overlap with exact output
and guard checks. This is not yet a combined disk-to-game boot or a test of
accelerator instruction-cache visibility.

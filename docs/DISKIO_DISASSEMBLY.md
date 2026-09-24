# Disk driver disassembly

`DiskIO.s`, `DosIO.s` and `DosIOR.s` replace the supplied encoded longwords
with Motorola-syntax 68000 instructions. Original notices and API comments are
preserved. Instructions, PC-relative references and writable data retain their
original positions. Numeric control-flow label suffixes are original offsets;
descriptive names and comments describe observed operations, not original names.

`DosIO.s` includes `DiskIO.s` and intentionally falls through into `diskio` at
the end of its sector-transfer wrapper. Assemble with `-I.` from the root,
and do not separately link DiskIO when using that combined module. `DosIOR.s`
contains its own distinct read-only driver at `diskior`; it is not replaced by
the full driver. Public labels `diskio`, `dosio`, `dosior`, `diskior` are retained.

The zero-filled filesystem state/name buffers and $ffff-initialized drive-state
words remain explicit data. PC-relative writes target these areas: the modules
require writable memory. No instruction fixes or functional changes are made.
`opt o-` is intentional: assembler optimizations can otherwise rewrite valid
instructions into different encodings even when instruction widths are given.
It leaves optimization disabled for subsequent source in the same assembly unit.

Validation:

```sh
make test-diskio-disassembly
```

The test assembles with vasm's normal defaults at two offsets and compares size
and SHA-256 with the original assembled modules. Original sizes are 1,916 bytes
(DiskIO), 3,570 bytes (DosIO including DiskIO), and 2,054 bytes (DosIOR including
its read-only driver). Initial conversion was also compared directly, byte by
byte, with original binaries. Baseline hashes and original-source hashes are in
`tests/diskio_original_hashes.json`; local original snapshots and binaries are in
`scratchpad/diskio_disassembly/`. These checks establish binary identity, not
new hardware validation of the historical routines.

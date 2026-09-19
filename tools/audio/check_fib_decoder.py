#!/usr/bin/env python3
"""Compile target C on host and check stored FIB1 streams against Python."""
import ctypes
import struct
import subprocess
import sys
from pathlib import Path
from codec_experiment import ROOT, fib_decode


def main():
    library = ROOT / 'out/fib_decode_host.dylib'
    subprocess.run(['cc', '-O2', '-shared', '-fPIC', str(ROOT/'fib_decode.c'),
                    '-o', str(library)], check=True)
    decode = ctypes.CDLL(str(library)).fib_decode_block
    decode.argtypes = [ctypes.c_void_p, ctypes.c_void_p, ctypes.c_uint]
    decode.restype = None
    bank = Path(sys.argv[1]).read_bytes()
    pos = count = 0
    while pos < len(bank):
        magic, samples, block = struct.unpack_from('<4sII', bank, pos)
        assert magic == b'FIB1' and block == 512 and samples > 0
        start = pos
        pos += 12
        parts = []
        for j in range(0, samples, 512):
            size = min(512, samples-j)
            length = 1 + size//2
            src = bank[pos:pos+length]
            assert len(src) == length
            pos += length
            dst = ctypes.create_string_buffer(size+2)
            dst[0], dst[size+1] = b'X', b'Y'
            decode(src, ctypes.byref(dst, 1), size)
            assert dst[0] == b'X' and dst[size+1] == b'Y'
            parts.append(dst.raw[1:size+1])
            count += 1
        assert b''.join(parts) == fib_decode(bank[start:pos]).tobytes()
    print(f'{count} blocks match Python; output canaries intact')


if __name__ == '__main__':
    main()

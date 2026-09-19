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
    # All packed codes and predictor values: verifies modulo wrap and the
    # cumulative two-sample table independently of the music corpus.
    delta = [-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21]
    for seed in range(256):
        for code in range(256):
            dst = ctypes.create_string_buffer(5)
            dst[0], dst[4] = b'X', b'Y'
            decode(bytes([seed,code]), ctypes.byref(dst,1), 3)
            a = (seed+delta[code>>4]) & 255
            b = (a+delta[code&15]) & 255
            assert dst.raw == bytes([88,seed,a,b,89])
    for n in range(1,513):
        src = bytes(range(256))+b'\xff'
        for alignment in (0,1):
            dst = ctypes.create_string_buffer(n+3)
            dst[alignment], dst[alignment+n+1] = b'X', b'Y'
            decode(src, ctypes.byref(dst,alignment+1), n)
            predictor = src[0]; expected = [predictor]
            for i in range(n-1):
                code = src[1+i//2]
                predictor = (predictor+delta[code&15 if i&1 else code>>4]) & 255
                expected.append(predictor)
            assert dst.raw[alignment+1:alignment+n+1] == bytes(expected)
            assert dst[alignment] == b'X' and dst[alignment+n+1] == b'Y'
    print('65,536 predictor/code pairs and all block lengths/alignment cases pass')
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

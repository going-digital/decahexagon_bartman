#!/usr/bin/env python3
"""Offline +6 dB gain with a soft knee for signed 8-bit Paula samples."""
import argparse
import struct
from pathlib import Path


def boost_pcm(pcm):
    # Double normal samples; smoothly compress peaks above 96 toward 127.
    # Silence stays exact, polarity is symmetric, and no sample can wrap.
    def boosted(byte):
        value = byte if byte < 128 else byte - 256
        magnitude = abs(value) * 2
        if magnitude > 96:
            excess = magnitude - 96
            magnitude = 96 + round(31 * excess / (31 + excess))
        return (magnitude if value >= 0 else -magnitude) & 255

    return pcm.translate(bytes(boosted(i) for i in range(256)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', help='input PCM bank stem (.pcm0/.pcm1)')
    parser.add_argument('output', help='output PCM bank stem')
    args = parser.parse_args()
    first = Path(args.source + '.pcm0').read_bytes()
    second = Path(args.source + '.pcm1').read_bytes()
    blob = first + second
    if len(first) < 32 or blob[:4] not in (b'FBP1', b'FBP2'):
        raise ValueError('Expected a split FBP1/FBP2 bank')
    data_offset, size = struct.unpack_from('>II', blob, 24)
    if not 32 <= data_offset <= len(first) or size != len(blob):
        raise ValueError('Invalid PCM bank layout')
    result = blob[:data_offset] + boost_pcm(blob[data_offset:])
    Path(args.output + '.pcm0').write_bytes(result[:len(first)])
    Path(args.output + '.pcm1').write_bytes(result[len(first):])


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Predecode exact duration variants offline; word-pad every physical slice."""
import json
import struct
import numpy as np
from codec_experiment import ROOT, fib_decode

p = ROOT / 'scratchpad/audio/courtesy/rate_test/12000'
m = json.loads((p / '192k.json').read_text())
bank = (p / '192k.bank').read_bytes()
edges = (p / '192k.edges.s8').read_bytes()
dictionary = [fib_decode(bank[a:b]).astype(np.int64)
              for a, b in zip(m['offsets'], m['offsets'][1:])]
entries, ids, sequence = [], {}, []

def add(key, data):
    if key not in ids:
        ids[key] = len(entries)
        entries.append(data + b'\0' * (len(data) & 1))
    sequence.append((ids[key], len(data)))

add('prefix', edges[:m['prefix_length']])
for ident, a, b in zip(m['dictionary_ids'], m['boundaries'], m['boundaries'][1:]):
    n = b - a
    src = dictionary[ident]
    # Exact rational interpolation, nearest-even; no float or second encoding.
    den = n - 1
    index, phase = np.divmod(np.arange(n, dtype=np.int64) * (len(src) - 1), den)
    numerator = src[index] * den + (src[np.minimum(index+1, len(src)-1)] - src[index]) * phase
    q, r = np.divmod(numerator, den)
    q += (r*2 > den) | ((r*2 == den) & ((q & 1) != 0))
    add((ident, n), q.astype(np.int8).tobytes())
add('suffix', edges[m['prefix_length']:])
offsets = [0]
for entry in entries:
    offsets.append(offsets[-1] + len(entry))
seqoff = 32 + 4*len(offsets)
dataoff = seqoff + 4*len(sequence)
size = dataoff + offsets[-1]
blob = struct.pack('>4s7I', b'FBP1', sum(n for _, n in sequence),
                   len(sequence), len(entries), 32, seqoff, dataoff, size)
blob += b''.join(struct.pack('>I', x) for x in offsets)
blob += b''.join(struct.pack('>HH', i, n) for i, n in sequence)
blob += b''.join(entries)
assert len(blob) == size
assert all((dataoff+x) % 2 == 0 for x in offsets)
(ROOT / 'out/courtesy.pcm').write_bytes(blob)
print(f'{len(entries)} word-aligned entries; {size} bytes packed PCM; {size+2048} with DMA buffers')

# Two separate Amiga load hunks fit 512 KiB Chip + 512 KiB expansion RAM.
split = dataoff + offsets[len(entries)//2]
(ROOT / "out/courtesy.pcm0").write_bytes(blob[:split])
(ROOT / "out/courtesy.pcm1").write_bytes(blob[split:])
print(f"Load chunks: {split} CPU bytes + {len(blob)-split} Chip bytes")

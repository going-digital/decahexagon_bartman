#!/usr/bin/env python3
"""Pack existing independently coded dictionary and exact segment durations."""
import json, struct
from codec_experiment import ROOT
p=ROOT/'scratchpad/audio/courtesy/rate_test/12000'
m=json.loads((p/'192k.json').read_text())
bank=(p/'192k.bank').read_bytes();edges=(p/'192k.edges.s8').read_bytes()
seq=[(65535,m['prefix_length'])]+list(zip(m['dictionary_ids'],[b-a for a,b in zip(m['boundaries'],m['boundaries'][1:])]))+[(65534,m['suffix_length'])]
assert all(0<n<65536 for _,n in seq)
# FBS2 aligns every encoded block as well as every dictionary/edge slice.
# Logical sample counts exclude padding. A 512-sample payload is 257 bytes.
padded = bytearray(); offsets = [0]
for a,b in zip(m['offsets'],m['offsets'][1:]):
    samples = struct.unpack_from('<I',bank,a+4)[0]
    padded += bank[a:a+12]; cursor = a+12
    for start in range(0,samples,512):
        size = 1+min(512,samples-start)//2
        padded += bank[cursor:cursor+size] + b'\0'*(size&1)
        cursor += size
    assert cursor == b
    offsets.append(len(padded))
bank = bytes(padded); m['offsets'] = offsets
prefix = edges[:m['prefix_length']]; suffix = edges[m['prefix_length']:]
edges = prefix+b'\0'*(len(prefix)&1)+suffix+b'\0'*(len(suffix)&1)
off=32;seqoff=off+4*len(m['offsets']);dataoff=seqoff+4*len(seq);edgeoff=dataoff+len(bank)
blob=struct.pack('>4s7I',b'FBS2',sum(n for _,n in seq),len(seq),len(m['offsets'])-1,off,seqoff,dataoff,edgeoff)
blob+=b''.join(struct.pack('>I',x) for x in m['offsets'])
blob+=b''.join(struct.pack('>HH',i,n) for i,n in seq)+bank+edges
(ROOT/'out/courtesy.fbs').write_bytes(blob)
print(len(blob),'packed bytes;',len(blob)+2560,'including four DMA buffers and decode cache')

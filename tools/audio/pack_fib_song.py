#!/usr/bin/env python3
"""Pack existing independently coded dictionary and exact segment durations."""
import json, struct
from codec_experiment import ROOT
p=ROOT/'scratchpad/audio/courtesy/rate_test/12000'
m=json.loads((p/'192k.json').read_text())
bank=(p/'192k.bank').read_bytes();edges=(p/'192k.edges.s8').read_bytes()
seq=[(65535,m['prefix_length'])]+list(zip(m['dictionary_ids'],[b-a for a,b in zip(m['boundaries'],m['boundaries'][1:])]))+[(65534,m['suffix_length'])]
assert all(0<n<65536 for _,n in seq)
off=32;seqoff=off+4*len(m['offsets']);dataoff=seqoff+4*len(seq);edgeoff=dataoff+len(bank)
blob=struct.pack('>4s7I',b'FBS1',sum(n for _,n in seq),len(seq),len(m['offsets'])-1,off,seqoff,dataoff,edgeoff)
blob+=b''.join(struct.pack('>I',x) for x in m['offsets'])
blob+=b''.join(struct.pack('>HH',i,n) for i,n in seq)+bank+edges
(ROOT/'out/courtesy.fbs').write_bytes(blob)
print(len(blob),'packed bytes;',len(blob)+2560,'including four DMA buffers and decode cache')

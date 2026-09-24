#!/usr/bin/env python3
"""Measure actual raw Zultra payload sizes; verify with independent zlib decode."""
import argparse, ctypes, hashlib, json, math, struct, subprocess, zlib
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';out.mkdir(exist_ok=True)
v=a.execram_source/'src/backends/zultra_vendor'
files=['blockdeflate.c','dictionary.c','frame.c','libzultra.c','matchfinder.c','huffman/bitwriter.c','huffman/huffencoder.c','huffman/huffutils.c','libdivsufsort/lib/divsufsort.c','libdivsufsort/lib/divsufsort_utils.c','libdivsufsort/lib/sssort.c','libdivsufsort/lib/trsort.c']
libpath=out/'libzultra.dylib'
subprocess.run(['cc','-O2','-shared','-fPIC','-I'+str(v),*[str(v/f) for f in files],'-o',str(libpath)],check=True)
lib=ctypes.CDLL(str(libpath));lib.zultra_memory_bound.argtypes=[ctypes.c_size_t,ctypes.c_uint,ctypes.c_uint];lib.zultra_memory_bound.restype=ctypes.c_size_t
lib.zultra_memory_compress.argtypes=[ctypes.c_void_p,ctypes.c_size_t,ctypes.c_void_p,ctypes.c_size_t,ctypes.c_uint,ctypes.c_uint];lib.zultra_memory_compress.restype=ctypes.c_size_t
import sys
sys.path.insert(0,str(root/'tools/audio'))
from fib_reference import fib_decode
from audio_sources import pc_source
rows=[]
for track in ['courtesy','focus','otis']:
 data=(root/f'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/{track}_expanded_fibonacci.aud').read_bytes()
 magic,n=struct.unpack_from('>4sI',data);assert magic==b'AUD1'
 m=json.loads(data[8:8+n]);assert m['gain_stage']=='before';fib=data[8+n:];pcm=fib_decode(fib)
 # Compact binary metadata, offsets refer to the decoded contiguous PCM bank.
 seq=m['sequence'];offsets=m['offsets']
 header=struct.pack('>4s5I',b'TUN1',m['rate'],m['samples'],len(offsets)-1,len(seq),len(pcm))
 header+=b''.join(struct.pack('>I',x) for x in offsets)
 header+=b''.join(struct.pack('>HH',i,count) for i,count in seq)
 assert offsets[-1]==len(pcm) and sum(count for _,count in seq)==m['samples']
 for codec,body in [('fibonacci',fib),('pcm',pcm)]:
  raw=header+body;dst=ctypes.create_string_buffer(lib.zultra_memory_bound(len(raw),0,0))
  size=lib.zultra_memory_compress(raw,len(raw),dst,len(dst),0,0);assert size<=len(dst)
  packed=dst.raw[:size];assert zlib.decompress(packed,-15)==raw
  (out/f'{track}.{codec}.deflate').write_bytes(packed)
  # Estimate OFS allocation: 488 data bytes/block, 72 block pointers/header.
  blocks=math.ceil(size/488);sectors=blocks+max(1,math.ceil(blocks/72))
  row=dict(source=pc_source(track),track=track,codec=codec,raw_bytes=len(raw),zultra_bytes=size,ofs_sectors=sectors,decoded_pcm_bytes=len(pcm),raw_sha256=hashlib.sha256(raw).hexdigest(),packed_sha256=hashlib.sha256(packed).hexdigest())
  rows.append(row);print(row,flush=True)
r={'gain_stage':'before_fibonacci','status':'Host size/round-trip evidence only; no target overlap or timing proof','execram_revision':subprocess.check_output(['git','-C',str(a.execram_source),'rev-parse','HEAD'],text=True).strip(),'vendor_source_sha256':{f:hashlib.sha256((v/f).read_bytes()).hexdigest() for f in files},'tunes':rows,'totals':{c:{'zultra_bytes':sum(x['zultra_bytes'] for x in rows if x['codec']==c),'ofs_sectors':sum(x['ofs_sectors'] for x in rows if x['codec']==c)} for c in ['fibonacci','pcm']}}
(root/'docs/TRACKLOADER_PAYLOAD_MEASUREMENTS.json').write_text(json.dumps(r,indent=2)+'\n')

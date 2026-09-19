#!/usr/bin/env python3
"""Trial shared-duration PCM dictionaries; no runtime codec or interpolation."""
import ctypes
import hashlib
import json
import struct
import subprocess
import numpy as np
from scipy.io import wavfile
from scipy.spatial.distance import cdist
from codec_experiment import ROOT, fib_decode
from reuse_beats import features, choose

source = ROOT/'scratchpad/audio/courtesy/rate_test/12000'
output = ROOT/'scratchpad/audio/courtesy/pcm_reuse'
output.mkdir(parents=True, exist_ok=True)
m = json.loads((source/'192k.json').read_text())
bank = (source/'192k.bank').read_bytes()
edges = (source/'192k.edges.s8').read_bytes()
slices = [fib_decode(bank[a:b]) for a,b in zip(m['offsets'],m['offsets'][1:])]
lengths = np.diff(m['boundaries']).tolist()
width = max(lengths)
assert all(abs(len(s)-width)<=1 for s in slices)
# Store one duration per instrument/phrase slice. Shorter uses omit its last
# sample; a short source is extended by repeating its final sample once.
canonical = [np.pad(s, (0,width-len(s)), mode='edge') for s in slices]
f = features(canonical, 12000)
distance = cdist(f,f,'sqeuclidean')/f.shape[1]
usage = np.bincount(m['dictionary_ids'], minlength=len(slices))
rate, accepted = wavfile.read(source/'192k_fibonacci.wav')
accepted = (accepted//256).astype(np.int8)
assert rate == 12000
subprocess.run(['cc','-O2','-shared','-fPIC',str(ROOT/'fib_pcm.c'),
                '-o',str(ROOT/'out/compact_pcm_host.dylib')],check=True)
lib = ctypes.CDLL(str(ROOT/'out/compact_pcm_host.dylib'))
lib.fib_pcm_init_split.argtypes=[ctypes.c_void_p,ctypes.c_void_p,ctypes.c_uint,ctypes.c_void_p,ctypes.c_uint]
lib.fib_song_read.argtypes=[ctypes.c_void_p,ctypes.c_void_p,ctypes.c_uint]
lib.fib_song_read.restype=None
accepted_jump_rms=float(np.sqrt(np.mean([(int(accepted[b])-int(accepted[b-1]))**2 for b in m['boundaries'] if 0<b<len(accepted)])))
reports=[]
for count in [134,128,112,96]:
    if count == len(slices):
        selected=list(range(count)); assignment=np.arange(count)
    else:
        selected,assignment=choose(distance*usage[None,:],count)
    raw=[edges[:m['prefix_length']]]+[canonical[i].tobytes() for i in selected]+[edges[m['prefix_length']:]]
    entries=[s+b'\0'*(len(s)&1) for s in raw]
    sequence=[(0,len(raw[0]))]+[(int(assignment[i])+1,n) for i,n in zip(m['dictionary_ids'],lengths)]+[(count+1,len(raw[-1]))]
    offsets=[0]
    for entry in entries: offsets.append(offsets[-1]+len(entry))
    seqoff=32+4*len(offsets);dataoff=seqoff+4*len(sequence)
    total=dataoff+offsets[-1]
    blob=struct.pack('>4s7I',b'FBP2',len(accepted),len(sequence),len(entries),32,seqoff,dataoff,total)
    blob+=b''.join(struct.pack('>I',x) for x in offsets)
    blob+=b''.join(struct.pack('>HH',i,n) for i,n in sequence)+b''.join(entries)
    split=dataoff+offsets[len(entries)//2]
    first,second=blob[:split],blob[split:]
    assert all((dataoff+x)%2==0 for x in offsets)
    expected=b''.join(raw[i][:n] for i,n in sequence)
    assert len(expected)==len(accepted)
    state=ctypes.create_string_buffer(2048)
    assert lib.fib_pcm_init_split(state,first,len(first),second,len(second))
    # Exercise byte, odd and DMA-sized reads across the split and loop boundary.
    want=expected+expected[:4096]; got=bytearray();pos=0
    while pos<len(want):
        n=min([1,511,512,17,1023][pos%5],len(want)-pos)
        dst=ctypes.create_string_buffer(n+2);dst[0]=b'X';dst[n+1]=b'Y'
        lib.fib_song_read(state,ctypes.byref(dst,1),n)
        assert dst[0]==b'X' and dst[n+1]==b'Y'
        got+=dst.raw[1:n+1];pos+=n
    assert got==want
    # Reject entries larger than the bounded shared-duration allowance.
    broken=bytearray(first);struct.pack_into('>H',broken,seqoff+4+2,1)
    assert not lib.fib_pcm_init_split(state,bytes(broken),len(first),second,len(second))
    stem=output/str(count)
    stem.with_suffix('.pcm').write_bytes(blob)
    stem.with_suffix('.pcm0').write_bytes(first)
    stem.with_suffix('.pcm1').write_bytes(second)
    q=np.frombuffer(expected,dtype=np.int8)
    wavfile.write(stem.with_suffix('.wav'),rate,q.astype(np.int16)*256)
    difference=q.astype(float)-accepted.astype(float)
    report={'dictionary_slices':count,'asset_bytes':total,'with_dma_buffers_bytes':total+2048,
            'cpu_chunk_bytes':len(first),'chip_chunk_bytes':len(second),
            'weighted_feature_mse_vs_full_dictionary':float(np.mean(distance[np.array(selected)[assignment[m['dictionary_ids']]],m['dictionary_ids']])),
            'sample_rmse_vs_accepted':float(np.sqrt(np.mean(difference**2))),
            'boundary_jump_rms':float(np.sqrt(np.mean([(int(q[b])-int(q[b-1]))**2 for b in m['boundaries'] if 0<b<len(q)]))),
            'samples':len(q),'sequence_timing_unchanged':True,'host_target_reader_matches':True,
            'sha256':hashlib.sha256(blob).hexdigest(),'preview':str(stem.with_suffix('.wav').relative_to(ROOT)),
            'source_slice_ids':selected,'assignment':assignment.tolist()}
    reports.append(report)
    print(count,total,'bytes; target reader matches; RMSE',round(report['sample_rmse_vs_accepted'],3))
(ROOT/'soundtrack/pcm_reuse_trials.json').write_text(json.dumps({'status':'host comparison; listening judgement required; no runtime decompression',
    'sample_rate':rate,'accepted_boundary_jump_rms':accepted_jump_rms,'source_bank_sha256':hashlib.sha256(bank).hexdigest(),'trials':reports},indent=2)+'\n')

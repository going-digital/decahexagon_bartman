#!/usr/bin/env python3
"""Verify compact PCM state against the accepted dictionary, including looping."""
import ctypes as C,struct,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
subprocess.run(['cc','-O2','-shared','-fPIC',str(ROOT/'fib_pcm.c'),'-o',str(ROOT/'out/pcm_memory_check.dylib')],check=True)
class State(C.Structure):
    _fields_=[(n,C.c_void_p) for n in ('sequence','offsets','bank','edges','raw')]+[('pcm_split',C.c_uint)]+[(n,C.c_ushort) for n in ('seq_count','seq_index','output_left')]
class Guard(C.Structure):
    _fields_=[('before',C.c_uint64),('state',State),('after',C.c_uint64)]
prefix=ROOT/'scratchpad/audio/courtesy/pcm_reuse/96'
a=Path(str(prefix)+'.pcm0').read_bytes();b=Path(str(prefix)+'.pcm1').read_bytes();blob=a+b
magic,total,count,banks,off,seq,data,size=struct.unpack_from('>4s7I',blob)
assert magic==b'FBP2' and size==len(blob)
offsets=struct.unpack_from('>'+str(banks+1)+'I',blob,off)
reference=b''.join(blob[data+offsets[i]:data+offsets[i]+n] for i,n in struct.iter_unpack('>HH',blob[seq:data]))
assert len(reference)==total
lib=C.CDLL(str(ROOT/'out/pcm_memory_check.dylib'))
lib.fib_pcm_init_split.argtypes=[C.c_void_p,C.c_void_p,C.c_uint,C.c_void_p,C.c_uint]
lib.fib_song_read.argtypes=[C.c_void_p,C.c_void_p,C.c_uint]
state=Guard(0x12345678,State(),0x87654321)
assert lib.fib_pcm_init_split(C.byref(state.state),a,len(a),b,len(b))
expected=reference+reference[:4096];pos=0;sizes=(1,511,512,17,1023,3)
i=0
while pos<len(expected):
    n=min(sizes[i%len(sizes)],len(expected)-pos);out=C.create_string_buffer(n+2);out[0]=b'X';out[n+1]=b'Y'
    lib.fib_song_read(C.byref(state.state),C.byref(out,1),n)
    assert out.raw[1:n+1]==expected[pos:pos+n] and out[0]==b'X' and out[n+1]==b'Y'
    assert state.before==0x12345678 and state.after==0x87654321
    pos+=n;i+=1
print(pos,'samples match exactly across a full-song loop; state/output canaries intact')

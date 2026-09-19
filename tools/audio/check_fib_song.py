#!/usr/bin/env python3
"""Check the target sequencer against the accepted full-track preview."""
import ctypes,subprocess
import numpy as np
from scipy.io import wavfile
from codec_experiment import ROOT
subprocess.run(['cc','-O2','-shared','-fPIC',str(ROOT/'fib_song.c'),str(ROOT/'fib_decode.c'),'-o',str(ROOT/'out/fib_song_host.dylib')],check=True)
lib=ctypes.CDLL(str(ROOT/'out/fib_song_host.dylib'))
lib.fib_song_init.argtypes=[ctypes.c_void_p,ctypes.c_void_p,ctypes.c_uint]
lib.fib_song_read.argtypes=[ctypes.c_void_p,ctypes.c_void_p,ctypes.c_uint]
lib.fib_song_read.restype=None
blob=(ROOT/'out/courtesy.fbs').read_bytes()
s=ctypes.create_string_buffer(2048)
assert lib.fib_song_init(s,blob,len(blob))
sr,reference=wavfile.read(ROOT/'scratchpad/audio/courtesy/rate_test/12000/192k_fibonacci.wav')
ref=(reference//256).astype(np.int16)
result=[];remaining=len(ref)+4096
sizes=[1,256,511,512,17,1023,3]
i=0
while remaining:
 n=min(remaining,sizes[i%len(sizes)]);out=ctypes.create_string_buffer(n+2)
 out[0]=b'X';out[n+1]=b'Y'
 lib.fib_song_read(s,ctypes.byref(out,1),n)
 assert out[0]==b'X' and out[n+1]==b'Y'
 result.extend(np.frombuffer(out.raw[1:n+1],dtype=np.int8));remaining-=n;i+=1
q=np.array(result,dtype=np.int16);expected=np.r_[ref,ref[:4096]]
diff=q-expected
assert np.array_equal(q,expected)
print('samples',len(ref),'differences',np.count_nonzero(diff),'max error',abs(diff).max())
# Also check chunk partition invariance through the complete loop boundary.
s2=ctypes.create_string_buffer(2048);assert lib.fib_song_init(s2,blob,len(blob))
out=ctypes.create_string_buffer(len(q));lib.fib_song_read(s2,out,len(q))
assert np.array_equal(np.frombuffer(out.raw,dtype=np.int8),q)
for cut in [0,31,32,len(blob)//2,len(blob)-1]:
 assert not lib.fib_song_init(s2,blob,cut)
for pos in [0,4,8,12,16,20,24,28,32]:
 broken=bytearray(blob);broken[pos:pos+4]=b'\xff'*4
 assert not lib.fib_song_init(s2,bytes(broken),len(broken))
wavfile.write(ROOT/'out/courtesy_target_sequence.wav',sr,q[:len(ref)].astype(np.int16)*256)
print('Chunking, loop, canaries and invalid-layout checks passed')

# Independent small-vector interpolation checks, especially codec boundaries
# and the first/last-sample cases of the specialized +/- one-sample path.
import struct
from fractions import Fraction
from codec_experiment import fib_encode, fib_decode
rng=np.random.default_rng(9182)
for source_length in [2,3,4,17,511,512,513,520]:
    original=rng.integers(-128,128,source_length,dtype=np.int16).astype(np.int8)
    encoded=fib_encode(original);decoded=fib_decode(encoded).astype(int)
    for target_length in [source_length-1,source_length,source_length+1]:
        if target_length<2: continue
        packet=struct.pack('>4s7I',b'FBS1',target_length+2,3,1,32,40,52,52+len(encoded))
        packet+=struct.pack('>2I6H',0,len(encoded),65535,1,0,target_length,65534,1)+encoded+b'\x37\xb9'
        state=ctypes.create_string_buffer(2048)
        assert lib.fib_song_init(state,packet,len(packet))
        expected=[55]
        for i in range(target_length):
            pos=Fraction(i*(source_length-1),target_length-1)
            j=pos.numerator//pos.denominator
            a=int(decoded[j]);b=int(decoded[min(j+1,source_length-1)])
            expected.append(round(Fraction(a)+(b-a)*(pos-j)))
        expected.append(-71)
        # One-byte calls stress persistence at every possible boundary.
        for value in expected*2:
            result=ctypes.create_string_buffer(1)
            lib.fib_song_read(state,result,1)
            assert int.from_bytes(result.raw,'big',signed=True)==value
print('Small-vector and 511/512/513-sample codec-boundary checks passed')

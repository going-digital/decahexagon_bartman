#!/usr/bin/env python3
"""Block-optimal offline Fibonacci encoder comparison, unchanged FIB1 decoder."""
import ctypes,itertools,json,struct,subprocess
import numpy as np
from scipy.io import wavfile
from codec_experiment import ROOT,DELTA,fib_encode,fib_decode,metric


def load_encoder():
    library=ROOT/'out/fib_optimal.dylib'
    subprocess.run(['cc','-O3','-shared','-fPIC',str(ROOT/'tools/audio/fib_optimal.c'),'-o',str(library)],check=True)
    lib=ctypes.CDLL(str(library)); fn=lib.fib_optimal
    fn.argtypes=[ctypes.POINTER(ctypes.c_int8),ctypes.c_int,ctypes.POINTER(ctypes.c_uint8)]
    fn.restype=ctypes.c_int
    return fn


def encode(q,fn):
    q=np.asarray(q,dtype=np.int8)
    data=bytearray(struct.pack('<4sII',b'FIB1',len(q),512));total=0
    for start in range(0,len(q),512):
        block=np.ascontiguousarray(q[start:start+512]);codes=np.zeros(len(block)-1,dtype=np.uint8)
        error=fn(block.ctypes.data_as(ctypes.POINTER(ctypes.c_int8)),len(block),codes.ctypes.data_as(ctypes.POINTER(ctypes.c_uint8)))
        assert error>=0;total+=error
        data.append(int(block[0])&255)
        if len(codes)%2:codes=np.r_[codes,np.uint8(8)]
        data.extend(((codes[::2]<<4)|codes[1::2]).tobytes())
    decoded=fib_decode(data)
    assert int(np.sum((decoded.astype(np.int64)-q)**2))==total
    return bytes(data)


def main():
    fn=load_encoder()
    # Brute-force independent tiny-path oracle, including signed-wrap jumps.
    for q in [np.array([127,-100,15,-128],dtype=np.int8),np.array([0,30,60,90],dtype=np.int8)]:
        best=10**9
        for path in itertools.product(range(16),repeat=3):
            p=int(q[0]);cost=0
            for target,c in zip(q[1:],path):
                p=(p+int(DELTA[c])+128)%256-128;cost+=(p-int(target))**2
            best=min(best,cost)
        y=fib_decode(encode(q,fn));assert np.sum((y.astype(np.int64)-q)**2)==best
    for n in [1,2,511,512,513,1025]:
        q=np.random.default_rng(n).integers(-128,128,n,dtype=np.int16).astype(np.int8)
        a=fib_encode(q);b=encode(q,fn)
        assert len(a)==len(b)
        assert np.sum((fib_decode(b).astype(np.int64)-q)**2)<=np.sum((fib_decode(a).astype(np.int64)-q)**2)
    report={'objective':'minimum unweighted squared sample error per 512-sample block, first predictor fixed',
            'decoder_format':'FIB1 unchanged','validation':'brute-force four-sample oracle and reset/wrap boundary cases pass','tracks':[]}
    for name in ['courtesy','focus','otis']:
        work=ROOT/'scratchpad/audio'/name/'codec_test'
        sr,pcm=wavfile.read(work/'reference.wav');q=(pcm//256).astype(np.int8)
        greedy=fib_encode(q);optimal=encode(q,fn)
        assert len(greedy)==len(optimal)
        (work/'fibonacci_optimal.bin').write_bytes(optimal)
        a=fib_decode(greedy);b=fib_decode(optimal)
        wavfile.write(work/'fibonacci_optimal_preview.wav',sr,b.astype(np.int16)*256)
        row={'track':name,'bytes':len(optimal),'greedy':metric(q,a),'optimal':metric(q,b)}
        report['tracks'].append(row);print(row,flush=True)
    (ROOT/'soundtrack/fibonacci_encoder_comparison.json').write_text(json.dumps(report,indent=2)+'\n')

if __name__=='__main__':main()

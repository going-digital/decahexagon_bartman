#!/usr/bin/env python3
"""Host codec quality/storage experiment. No A500 speed claim."""
import json,struct,subprocess
import numpy as np
from scipy import signal
from scipy.cluster.vq import kmeans2
from scipy.spatial.distance import cdist
from scipy.io import wavfile
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
RATE=16000
DELTA=np.array([-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21])


def fib_encode(q):
    data=bytearray(struct.pack('<4sII',b'FIB1',len(q),512))
    for start in range(0,len(q),512):
        block=q[start:start+512];pred=int(block[0]);data.append(pred&255)
        codes=[]
        for target in block[1:]:
            candidates=(pred+DELTA+128)%256-128
            code=int(np.argmin((candidates-int(target))**2))
            codes.append(code);pred=int(candidates[code])
        if len(codes)%2:codes.append(8)
        data.extend((codes[i]<<4)|codes[i+1] for i in range(0,len(codes),2))
    return bytes(data)


def fib_decode(data):
    magic,count,blocksize=struct.unpack('<4sII',data[:12]);assert magic==b'FIB1'
    out=[];p=12
    while len(out)<count:
        n=min(blocksize,count-len(out));pred=int.from_bytes(data[p:p+1],'little',signed=True);p+=1
        out.append(pred)
        for i in range(n-1):
            code=(data[p+i//2]>>(4 if i%2==0 else 0))&15
            pred=(pred+int(DELTA[code])+128)%256-128;out.append(pred)
        p+=(n-1+1)//2
    assert p==len(data)
    return np.array(out,dtype=np.int8)


def vq_encode(q):
    assert len(q)%8==0
    x=q.reshape(-1,8).astype(float)
    # Fit only first half, evaluate second half as unseen audio too.
    training=x[:len(x)//2]
    rng=np.random.default_rng(42)
    subset=training[rng.choice(len(training),min(5000,len(training)),replace=False)]
    bank,_=kmeans2(subset,256,iter=15,minit='++',seed=42)
    bank=np.clip(np.rint(bank),-128,127).astype(np.int8)
    ids=np.concatenate([np.argmin(cdist(chunk,bank,'sqeuclidean'),axis=1)
                        for chunk in np.array_split(x,16)]).astype(np.uint8)
    data=struct.pack('<4sI',b'VQ08',len(q))+bank.tobytes()+ids.tobytes()
    return data


def vq_decode(data):
    magic,count=struct.unpack('<4sI',data[:8]);assert magic==b'VQ08'
    bank=np.frombuffer(data[8:2056],dtype=np.int8).reshape(256,8)
    ids=np.frombuffer(data[2056:],dtype=np.uint8)
    out=bank[ids].ravel();assert len(out)==count
    return out


def metric(x,y):
    x=x.astype(float);y=y.astype(float)
    err=np.sum((x-y)**2)
    _,_,a=signal.stft(x,nperseg=512,noverlap=384)
    _,_,b=signal.stft(y,nperseg=512,noverlap=384)
    return {'snr_db':float(10*np.log10(max(np.sum(x*x),1e-12)/max(err,1e-12))),
            'relative_spectral_error':float(np.linalg.norm(abs(a)-abs(b))/np.linalg.norm(abs(a)))}


def main():
    reports=[]
    for name in ['courtesy','focus','otis']:
        work=ROOT/'scratchpad/audio'/name
        out=work/'codec_test';out.mkdir(exist_ok=True)
        sr,stereo=wavfile.read(work/'decoded.wav')
        x=signal.resample_poly(stereo[:20*sr].mean(axis=1),RATE,sr)
        gain=min(1.,.98/max(abs(x).max(),1e-9))
        q=np.clip(np.rint(x*gain*127),-128,127).astype(np.int8)
        reference=out/'reference.wav';wavfile.write(reference,RATE,q.astype(np.int16)*256)
        variants={}
        for codec,encoder,decoder in [('fibonacci',fib_encode,fib_decode),('vq8',vq_encode,vq_decode)]:
            data=encoder(q);(out/f'{codec}.bin').write_bytes(data)
            variants[codec]=(decoder(data),len(data))
        packed=out/'ima_adpcm.wav'
        subprocess.run(['ffmpeg','-v','error','-y','-i',str(reference),'-c:a','adpcm_ima_wav',str(packed)],check=True)
        decoded=out/'ima_decoded16.wav'
        subprocess.run(['ffmpeg','-v','error','-y','-i',str(packed),'-c:a','pcm_s16le',str(decoded)],check=True)
        ds,d=wavfile.read(decoded);assert ds==RATE and len(d)>=len(q)
        # Model final 8-bit Paula output; report padded storage, trim preview only.
        variants['ima_adpcm']=(np.clip(np.rint(d[:len(q)].astype(float)/256),-128,127).astype(np.int8),packed.stat().st_size)
        report={'track':name,'interval_seconds':[0,20],'sample_rate':RATE,'reference_bytes':len(q),
                'common_gain':float(gain),'vq_training_seconds':[0,10],'variants':{}}
        for codec,(y,size) in variants.items():
            assert len(y)==len(q)
            wavfile.write(out/f'{codec}_preview.wav',RATE,y.astype(np.int16)*256)
            report['variants'][codec]={'encoded_bytes_including_headers':size,'compression_ratio':len(q)/size,
                                      'whole_excerpt':metric(q,y),'second_half':metric(q[len(q)//2:],y[len(q)//2:])}
        reports.append(report);print(json.dumps(report),flush=True)
    # Variable-length block boundaries and full-range input exercise both formats.
    for n in [1,2,511,512,513,1025]:
        test=np.resize(np.array([-128,127,0,-1,1],dtype=np.int8),n)
        assert len(fib_decode(fib_encode(test)))==n
    (ROOT/'soundtrack/codec_comparison.json').write_text(json.dumps({'status':'host quality/storage only; no target timing measured',
        'ffmpeg':subprocess.check_output(['ffmpeg','-version'],text=True).splitlines()[0], 'tracks':reports},indent=2)+'\n')

if __name__=='__main__':main()

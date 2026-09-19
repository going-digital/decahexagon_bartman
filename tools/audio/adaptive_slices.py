#!/usr/bin/env python3
"""12 kHz shared half-beat bank plus unique quarter/half-beat replacements.
A host experiment: replacements overwrite, never mix with, base playback.
"""
import json
import numpy as np
from scipy.io import wavfile
from scipy.spatial.distance import cdist
import reuse_beats as reuse
from codec_experiment import fib_encode,fib_decode
from compare_fib_encoders import load_encoder,encode
ROOT=reuse.ROOT


def main():
    work=ROOT/'scratchpad/audio/courtesy';out=work/'adaptive_12k';out.mkdir(exist_ok=True)
    sr,pcm=wavfile.read(work/'rate_test/12000/reference.wav');assert sr==12000
    q=(pcm//256).astype(np.int8)
    info=json.loads((ROOT/'soundtrack/courtesy.analysis.json').read_text())['constant_grid_hypothesis']
    edges=np.rint(np.arange(info['phase_seconds'],len(q)/sr,30/info['bpm'])*sr).astype(int)
    slices=[q[a:b] for a,b in zip(edges[:-1],edges[1:])]
    features=reuse.features(slices,sr);distance=cdist(features,features,'sqeuclidean')
    fn=load_encoder();report={'status':'host only, unreviewed variable-length replacements', 'sample_rate':sr,'trials':[]}
    for reserve in [16,32]:
        budget=192*1024
        # Same base accounting as compressed bank, plus 16B replacement-stream header.
        fixed=int(edges[0]+len(q)-edges[-1]+32+4*(len(slices)+2)+1536+16)
        slot=max(len(fib_encode(np.zeros(n,dtype=np.int8))) for n in set(map(len,slices)))
        k=int((budget-fixed-4-reserve*1024)//(slot+4))
        chosen,ids=reuse.choose(distance,k)
        basebank=bytearray();offsets=[];decoded=[]
        for index in chosen:
            offsets.append(len(basebank));blob=encode(slices[index],fn);basebank.extend(blob);decoded.append(fib_decode(blob))
        offsets.append(len(basebank))
        current=q.copy()
        for i,(a,b) in enumerate(zip(edges[:-1],edges[1:])):
            current[a:b]=reuse.matched_length(decoded[ids[i]],b-a)
        used=fixed+len(basebank)+4*len(offsets)
        candidates=[]
        for i,(a,b) in enumerate(zip(edges[:-1],edges[1:])):
            original=slices[i];base=current[a:b]
            old=float(np.mean((reuse.features([base],sr)[0]-features[i])**2))
            mid=len(original)//2
            for lo,hi in [(0,mid),(mid,len(original)),(0,len(original))]:
                # Independent codecs of identical target audio, scored after decoding.
                target=original[lo:hi];blob=encode(target,fn)
                for codec,payload,predicted in [('fib',blob,fib_decode(blob)),('pcm',target.tobytes(),target)]:
                    trial=base.copy();trial[lo:hi]=predicted
                    new=float(np.mean((reuse.features([trial],sr)[0]-features[i])**2))
                    benefit=old-new;cost=len(payload)+16 # start,length,offset,length+codec packed descriptor
                    if benefit>0:
                        candidates.append((benefit/cost,i,int(a+lo),int(a+hi),codec,payload,benefit))
        candidates.sort(key=lambda c:c[0],reverse=True)
        patchbank=bytearray();patches=[];touched=set()
        # One replacement per half beat avoids counting overlapping improvements.
        for _,i,a,b,codec,payload,benefit in candidates:
            if i in touched or used+len(payload)+16>budget:continue
            touched.add(i);offset=len(patchbank);patchbank.extend(payload)
            current[a:b]=fib_decode(payload) if codec=='fib' else np.frombuffer(payload,dtype=np.int8)
            patches.append({'start':a,'samples':b-a,'codec':codec,'offset':offset,'bytes':len(payload)})
            used+=len(payload)+16
        tag=f'reserve{reserve}k'
        (out/f'{tag}.base').write_bytes(basebank);(out/f'{tag}.patches').write_bytes(patchbank)
        (out/f'{tag}.edges').write_bytes(q[:edges[0]].tobytes()+q[edges[-1]:].tobytes())
        plan={'sample_rate':sr,'offsets':offsets,'ids':ids.tolist(),'edges':edges.tolist(),
              'prefix_samples':int(edges[0]),'patches':patches}
        (out/f'{tag}.json').write_text(json.dumps(plan)+'\n')
        wavfile.write(out/f'{tag}.wav',sr,current.astype(np.int16)*256)
        f=reuse.features([current[a:b] for a,b in zip(edges[:-1],edges[1:])],sr)
        # Same common-reference metric used by rate experiment, including bandwidth.
        s,ref=wavfile.read(work/'beat_reuse/reference_16k_8bit.wav')
        from scipy import signal
        common=signal.resample_poly(current.astype(float),s,sr)[:len(ref)]
        e16=np.rint(np.arange(info['phase_seconds'],len(ref)/s,30/info['bpm'])*s).astype(int)
        rf=reuse.features([ref[a:b].astype(float)/256 for a,b in zip(e16[:-1],e16[1:])])
        cf=reuse.features([common[a:b] for a,b in zip(e16[:-1],e16[1:])])
        row={'reserve_kib':reserve,'base_entries':k,'replacements':len(patches),
             'pcm_replacements':sum(p['codec']=='pcm' for p in patches),'accounted_bytes':used,
             'local_feature_mse':float(np.mean((f-features)**2)),
             'common_16k_feature_mse':float(np.mean((cf-rf)**2))}
        report['trials'].append(row);print(row,flush=True)
        assert used<=budget and len(current)==len(q)
    (ROOT/'soundtrack/courtesy.adaptive_slices.json').write_text(json.dumps(report,indent=2)+'\n')

if __name__=='__main__':main()

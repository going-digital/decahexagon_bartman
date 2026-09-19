#!/usr/bin/env python3
"""Controlled cross-track reuse test using equal-length, unstretched fragments.
Changes granularity from beat slices; comparisons are between these two controls.
"""
import json
import numpy as np
from scipy import signal
from scipy.io import wavfile
from scipy.spatial.distance import cdist
from compare_fib_encoders import load_encoder,encode
from codec_experiment import ROOT,fib_decode,fib_encode
from reuse_beats import features
RATE=12000
LENGTH=1024


def choose(distance,k,weights):
    # Start with one representative per track to make restricted distances finite.
    selected=[]
    nearest=np.full(distance.shape[1],np.inf)
    for track in range(3):
        rows=np.flatnonzero(CANDIDATE_TRACK==track)
        targets=TARGET_TRACK==track
        j=int(rows[np.argmin(distance[rows][:,targets].mean(axis=1))])
        selected.append(j);nearest=np.minimum(nearest,distance[j])
    for _ in range(k-len(selected)):
        scores=np.minimum(distance,nearest[None,:])@weights
        scores[selected]=np.inf
        j=int(np.argmin(scores));selected.append(j);nearest=np.minimum(nearest,distance[j])
    assignment=np.argmin(distance[selected],axis=0)
    return selected,assignment


def main():
    global CANDIDATE_TRACK,TARGET_TRACK
    out=ROOT/'scratchpad/audio/cross_track';out.mkdir(exist_ok=True)
    tracks=[];blocks=[];origins=[];candidate=[];fixed=1536+32
    for tid,name in enumerate(['courtesy','focus','otis']):
        sr,stereo=wavfile.read(ROOT/'scratchpad/audio'/name/'decoded.wav')
        x=signal.resample_poly(stereo.mean(axis=1),RATE,sr)
        gain=min(1.,.98/max(float(abs(x).max()),1e-9))
        q=np.clip(np.rint(x*gain*127),-128,127).astype(np.int8)
        n=len(q)//LENGTH;start=len(blocks)
        blocks.extend(q[:n*LENGTH].reshape(n,LENGTH));origins.extend([tid]*n)
        # Bounded evenly spaced candidate pool; every target still participates.
        candidate.extend((start+np.unique(np.linspace(0,n-1,min(256,n)).astype(int))).tolist())
        tail=q[n*LENGTH:]
        fixed+=len(tail)+16+4*(n+1)
        tracks.append({'name':name,'samples':len(q),'gain':gain,'start':start,'count':n,'tail':tail,'reference':q})
        wavfile.write(out/f'{name}_reference.wav',RATE,q.astype(np.int16)*256)
    blocks=np.array(blocks);candidate=np.array(candidate);TARGET_TRACK=np.array(origins);CANDIDATE_TRACK=TARGET_TRACK[candidate]
    descriptors=features(blocks,RATE)
    distance=cdist(descriptors[candidate],descriptors,'sqeuclidean')
    weights=np.array([1/(3*tracks[t]['count']) for t in origins])
    cost=len(fib_encode(np.zeros(LENGTH,dtype=np.int8)))+4
    k=int((192*1024-fixed-4)//cost);assert k>=3
    fn=load_encoder();cache={};report={'status':'host-only fixed-fragment comparison; not approved beat-bank replacement',
        'rate':RATE,'fragment_samples':LENGTH,'candidate_limit_per_track':256,'budget_kib':192,'entries':k,'trials':[]}
    for shared in [False,True]:
        mode='shared' if shared else 'restricted'
        d=distance.copy()
        if not shared:d[CANDIDATE_TRACK[:,None]!=TARGET_TRACK[None,:]]=np.inf
        selected,ids=choose(d,k,weights)
        bank=bytearray();offsets=[];decoded=[]
        for j in candidate[selected]:
            j=int(j)
            if j not in cache:cache[j]=encode(blocks[j],fn)
            offsets.append(len(bank));bank.extend(cache[j]);decoded.append(fib_decode(cache[j]))
        offsets.append(len(bank));decoded=np.array(decoded)
        (out/f'{mode}.bank').write_bytes(bank)
        plans=[];rows=[]
        for tid,track in enumerate(tracks):
            start,n=track['start'],track['count'];assigned=ids[start:start+n]
            recon=np.r_[decoded[assigned].ravel(),track['tail']]
            assert len(recon)==track['samples']
            wavfile.write(out/f"{track['name']}_{mode}.wav",RATE,recon.astype(np.int16)*256)
            (out/f"{track['name']}.tail").write_bytes(track['tail'].tobytes())
            f=features(decoded[assigned],RATE)
            from_other=CANDIDATE_TRACK[np.array(selected)[assigned]]!=tid
            rows.append({'track':track['name'],'gain':track['gain'],'cross_track_assignments':int(sum(from_other)),
                         'total_fragments':n,'feature_mse':float(np.mean((f-descriptors[start:start+n])**2))})
            plans.append({'track':track['name'],'ids':assigned.tolist(),'tail_bytes':len(track['tail'])})
        (out/f'{mode}.json').write_text(json.dumps({'offsets':offsets,'source_global_indices':candidate[selected].tolist(),
            'fragment_samples':LENGTH,'tracks':plans})+'\n')
        used=fixed+len(bank)+4*len(offsets);assert used<=192*1024
        row={'mode':mode,'accounted_bytes':used,'tracks':rows};report['trials'].append(row);print(row,flush=True)
    (ROOT/'soundtrack/cross_track_reuse.json').write_text(json.dumps(report,indent=2)+'\n')

if __name__=='__main__':main()

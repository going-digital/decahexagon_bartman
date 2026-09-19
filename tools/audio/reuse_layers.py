#!/usr/bin/env python3
"""Additive waveform dictionaries on the approved half-beat grid; host experiment."""
import json
import numpy as np
from scipy.spatial.distance import cdist
from scipy.io import wavfile
from scipy import signal
import reuse_beats as prior


def nearest(x,bank):
    return np.argmin(cdist(x,bank,'sqeuclidean'),axis=1)


def train(x,k0,k1=0):
    # Deterministic farthest-first initialization followed by alternating Lloyd
    # updates. Original grid alignment only; no independent carrier phase shifts.
    def init(a,k):
        chosen=[int(np.argmax(np.sum(a*a,axis=1)))]
        d=cdist(a,a[chosen],'sqeuclidean')[:,0]
        for _ in range(1,k):
            j=int(np.argmax(d));chosen.append(j)
            d=np.minimum(d,cdist(a,a[j:j+1],'sqeuclidean')[:,0])
        return a[chosen].copy()
    def update(target,bank):
        ids=nearest(target,bank)
        for j in range(len(bank)):
            if np.any(ids==j):bank[j]=target[ids==j].mean(axis=0)
        # Each bank really is signed 8-bit; clipping is part of the optimizer.
        bank[:]=np.clip(np.rint(bank),-128,127)
        return nearest(target,bank)
    b=init(x,k0);bi=nearest(x,b)
    c=init(x-b[bi],k1) if k1 else np.zeros((1,x.shape[1]))
    c=np.clip(np.rint(c),-128,127);ci=nearest(x-b[bi],c)
    for _ in range(10):
        bi=update(x-c[ci],b)
        if k1:ci=update(x-b[bi],c)
    return b,c,bi,ci


def main():
    root=prior.ROOT;work=root/'scratchpad/audio/courtesy'
    out=work/'layer_reuse';out.mkdir(exist_ok=True)
    sr,pcm=wavfile.read(work/'beat_reuse/reference_16k_8bit.wav')
    assert sr==prior.RATE
    q=(pcm//256).astype(np.int8)
    info=json.loads((root/'soundtrack/courtesy.analysis.json').read_text())
    g=info['constant_grid_hypothesis']
    edges=np.rint(np.arange(g['phase_seconds'],len(q)/sr,30/g['bpm'])*sr).astype(int)
    lengths=np.diff(edges);n=int(max(lengths))
    x=np.array([prior.matched_length(q[a:b],n) for a,b in zip(edges[:-1],edges[1:])],dtype=float)
    slot=(n+1)//2*2+2
    fixed=(edges[0]+1)//2*2+2+(len(q)-edges[-1]+1)//2*2+2
    reference_features=prior.features([q[a:b] for a,b in zip(edges[:-1],edges[1:])])
    report={'status':'unreviewed lossy additive dictionary; no source separation or target player',
            'sample_rate':sr,'beats_per_slice':.5,'iterations':10,'trials':[]}
    # A common half-gain leaves headroom for summing two full-range layers.
    wavfile.write(out/'reference.wav',sr,q.astype(np.float32)/256)
    previous=json.loads((root/'soundtrack/courtesy.beat_reuse.json').read_text())
    report['previous_spectral_dictionary']=[e for e in previous['experiments'] if e['beats_per_slice']==.5][0]['budgets']
    for budget in [64,192]:
        previous_rate,previous_pcm=wavfile.read(work/f'beat_reuse/0.5beat_{budget}k.wav')
        assert previous_rate==sr and len(previous_pcm)==len(q)
        wavfile.write(out/f'previous_{budget}k.wav',sr,previous_pcm.astype(np.float32)/65536)
    for budget in [64,192]:
        for ratio in [1.,.5,.75]:
            layers=1 if ratio==1 else 2
            sequence=(4 if layers==1 else 6)*(len(x)+2)+32
            count=int((budget*1024-fixed-sequence)//slot)
            k0=count if layers==1 else int(count*ratio);k1=count-k0
            b,c,bi,ci=train(x,k0,k1)
            first=q.astype(float);second=np.zeros(len(q))
            for i,(a,z) in enumerate(zip(edges[:-1],edges[1:])):
                first[a:z]=prior.matched_length(b[bi[i]].astype(np.int8),z-a)
                second[a:z]=prior.matched_length(c[ci[i]].astype(np.int8),z-a)
            mixed=first+second
            name=f'{budget}k_{k0}base_{k1}correction'
            for suffix,y in [('mix',mixed),('base',first),('correction',second)]:
                assert np.isfinite(y).all() and max(abs(y))<=256
                wavfile.write(out/f'{name}_{suffix}.wav',sr,(y/256).astype(np.float32))
            # Save actual quantized dictionaries; incomplete edges stored separately.
            bank=bytearray()
            for fragment in [q[:edges[0]],q[edges[-1]:]]:
                bank.extend(fragment.tobytes());bank.extend(bytes(len(fragment)%2+2))
            for row in np.vstack([b,c]) if k1 else b:
                bank.extend(row.astype(np.int8).tobytes());bank.extend(bytes(slot-n))
            assert len(bank)+sequence<=budget*1024
            (out/f'{name}.s8').write_bytes(bank)
            (out/f'{name}.json').write_text(json.dumps({'base_ids':bi.tolist(),
                'correction_ids':ci.tolist() if k1 else [],'boundaries':edges.tolist(),
                'base_count':k0,'correction_count':k1,'slot_bytes':slot,'active_length':n,
                'prefix_samples':int(edges[0]),'suffix_samples':int(len(q)-edges[-1])})+'\n')
            f=prior.features([mixed[a:z] for a,z in zip(edges[:-1],edges[1:])])
            result={'name':name,'layers':layers,'budget_kib':budget,'base_slices':k0,
                'correction_slices':k1,'accounted_bytes':len(bank)+sequence,
                'relative_waveform_error':float(np.linalg.norm(mixed-q)/np.linalg.norm(q.astype(float))),
                'feature_mse':float(np.mean((f-reference_features)**2)),
                'preview':str((out/f'{name}_mix.wav').relative_to(root))}
            report['trials'].append(result);print(result,flush=True)
    (root/'soundtrack/courtesy.layer_reuse.json').write_text(json.dumps(report,indent=2)+'\n')

if __name__=='__main__':main()

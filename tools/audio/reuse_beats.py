#!/usr/bin/env python3
"""Measure lossy beat-slice reuse; writes host previews, not Amiga-ready assets.
Fixed mono 16 kHz signed 8-bit dictionary, unmodified approved beat-grid origin.
"""
import hashlib
import json
from pathlib import Path
import numpy as np
from scipy import signal
from scipy.spatial.distance import cdist
from scipy.io import wavfile

ROOT=Path(__file__).resolve().parents[2]
RATE=16000
BUDGETS=[32,64,128,192]


def features(slices, sample_rate=RATE):
    rows=[]
    for s in slices:
        x=s.astype(float)/128
        f,t,z=signal.stft(x,sample_rate,nperseg=512,noverlap=384)
        # Preserve both loudness and evolving spectral content. Warping descriptor
        # time is only for comparison; all candidates have the same beat duration.
        bands=np.geomspace(40,min(8000,sample_rate/2),49)
        descriptor=[]
        for a,b in zip(bands[:-1],bands[1:]):
            bins=(f>=a)&(f<b)
            energy=np.sqrt(np.mean(abs(z[bins])**2,axis=0)) if bins.any() else np.zeros(len(t))
            descriptor.append(np.interp(np.linspace(t[0],t[-1],24),t,np.log1p(energy*100)))
        rows.append(np.asarray(descriptor).ravel())
    return np.asarray(rows)


def choose(distance,count):
    # Greedy facility-location: each added real slice minimizes aggregate error.
    # This is a heuristic, not a globally optimal or perceptually verified bank.
    selected=[]
    nearest=np.full(len(distance),np.inf)
    for _ in range(min(count,len(distance))):
        costs=np.minimum(nearest[None,:],distance).sum(axis=1)
        costs[selected]=np.inf
        index=int(np.argmin(costs))
        selected.append(index)
        nearest=np.minimum(nearest,distance[index])
    assignment=np.argmin(distance[selected],axis=0)
    return selected,assignment


def matched_length(s,n):
    if len(s)==n: return s.copy()
    # Rounded absolute beat boundaries differ by at most a sample. No arbitrary
    # phrase stretching: render this tiny correction explicitly for the preview.
    assert abs(len(s)-n)<=1
    return np.clip(np.rint(np.interp(np.linspace(0,len(s)-1,n),np.arange(len(s)),s)), -128,127).astype(np.int8)


def main():
    work=ROOT/'scratchpad/audio/courtesy'
    out=work/'beat_reuse';out.mkdir(exist_ok=True)
    metadata=json.loads((ROOT/'soundtrack/courtesy.analysis.json').read_text())
    sr,stereo=wavfile.read(work/'decoded.wav')
    x=signal.resample_poly(stereo.mean(axis=1),RATE,sr)
    # One common gain for every output. No per-slice normalization masks mistakes.
    gain=min(1.,.98/max(float(abs(x).max()),1e-9))
    q=np.clip(np.rint(x*gain*127),-128,127).astype(np.int8)
    wavfile.write(out/'reference_16k_8bit.wav',RATE,q.astype(np.int16)*256)
    bpm=metadata['constant_grid_hypothesis']['bpm']
    phase=metadata['constant_grid_hypothesis']['phase_seconds']
    report={'source_sha256':metadata['sha256'],'sample_rate':RATE,'channels':1,
            'sample_format':'signed 8-bit; WAV previews use lossless int16 container',
            'common_gain':gain,'original_samples':len(q),'bpm':bpm,'phase_seconds':phase,
            'status':'offline lossy reuse experiment; no listening approval or target playback',
            'experiments':[]}
    for beats in [.5,1.,2.]:
        edges=np.rint(np.arange(phase,len(q)/RATE,60/bpm*beats)*RATE).astype(int)
        slices=[q[a:b] for a,b in zip(edges[:-1],edges[1:])]
        prefix=q[:edges[0]];suffix=q[edges[-1]:]
        # Retain incomplete intro/outro verbatim. Conservative max-length slots,
        # rounded to Paula word boundaries, include one guard word per sample.
        slot_bytes=((max(map(len,slices))+1)//2)*2+2
        fixed=((len(prefix)+1)//2)*2+2+((len(suffix)+1)//2)*2+2
        # Proposed sequence: sample ID u16 + output length u16 per slice, 32B header.
        # This is accounted storage, not a proven playback format/CPU budget.
        sequence_bytes=4*(len(slices)+2)+32
        descriptor=features(slices)
        distance=cdist(descriptor,descriptor,metric='sqeuclidean')
        hashes=[hashlib.sha256(s.tobytes()).hexdigest() for s in slices]
        entry={'beats_per_slice':beats,'slice_count':len(slices),'exact_unique_slices':len(set(hashes)),
               'slot_bytes':slot_bytes,'fixed_intro_outro_bytes':fixed,'sequence_bytes':sequence_bytes,'budgets':[]}
        for kib in BUDGETS:
            count=(kib*1024-fixed-sequence_bytes)//slot_bytes
            if count<1: continue
            selected,assignment=choose(distance,count)
            recon=q.copy()
            for i,a in enumerate(assignment):
                recon[edges[i]:edges[i+1]]=matched_length(slices[selected[a]],edges[i+1]-edges[i])
            assert len(recon)==len(q) and np.array_equal(recon[:edges[0]],prefix)
            assert np.array_equal(recon[edges[-1]:],suffix)
            error=np.linalg.norm(recon.astype(float)-q)/max(np.linalg.norm(q.astype(float)),1e-9)
            jumps=abs(recon[edges[1:-1]].astype(float)-recon[edges[1:-1]-1])
            refjumps=abs(q[edges[1:-1]].astype(float)-q[edges[1:-1]-1])
            tag=f'{beats:g}beat_{kib}k'
            wavfile.write(out/f'{tag}.wav',RATE,recon.astype(np.int16)*256)
            # Store the measured bank as signed bytes, with fixed-size slots.
            # Metadata specifies active lengths; guard/padding is not played.
            bank=bytearray()
            for fragment in [prefix,suffix]:
                bank.extend(fragment.tobytes())
                bank.extend(bytes((len(fragment)%2)+2))
            for index in selected:
                bank.extend(slices[index].tobytes())
                bank.extend(bytes(slot_bytes-len(slices[index])))
            assert len(bank)+sequence_bytes<=kib*1024
            (out/f'{tag}.s8').write_bytes(bank)
            plan={'bank_layout':'prefix, suffix (word padded + guard word), then fixed-size dictionary slots',
                  'prefix_samples':len(prefix),'suffix_samples':len(suffix),'slot_bytes':slot_bytes,
                  'dictionary_active_lengths':[len(slices[i]) for i in selected],
                  'source_slice_indices':selected,'sequence_dictionary_ids':assignment.tolist(),
                  'boundaries_samples':edges.tolist()}
            (out/f'{tag}.json').write_text(json.dumps(plan)+'\n')
            entry['budgets'].append({'budget_kib':kib,'dictionary_slices':len(selected),
              'estimated_bytes':fixed+sequence_bytes+len(selected)*slot_bytes,
              'weighted_feature_mse':float(distance[selected,np.arange(len(slices))[:,None]].min(axis=1).mean()/descriptor.shape[1]),
              'waveform_relative_error':float(error),
              'join_jump_p95_8bit_units':float(np.percentile(jumps,95)),
              'reference_join_jump_p95_8bit_units':float(np.percentile(refjumps,95)),
              'preview':str((out/f'{tag}.wav').relative_to(ROOT))})
        report['experiments'].append(entry)
        print(beats,'beats:',len(slices),'slices;',len(set(hashes)),'exact unique',flush=True)
    # Identity-bank control: dictionary playback must exactly recover rounded
    # boundaries including prefix/tail, even where individual lengths differ.
    reconstructed=np.concatenate([prefix]+[matched_length(s,len(s)) for s in slices]+[suffix])
    assert np.array_equal(q,reconstructed)
    # Repeated synthetic descriptors must be covered exactly by two medoids.
    toy=np.array([[0.,0.],[1.,1.],[0.,0.],[1.,1.]])
    td=cdist(toy,toy,metric='sqeuclidean')
    selected,assignment=choose(td,2)
    assert np.all(td[np.asarray(selected)[assignment],np.arange(4)]==0)
    report['validation']='repeated-descriptor control passes; identity bank reconstructs reference exactly; all previews retain sample count and edge fragments'
    (ROOT/'soundtrack/courtesy.beat_reuse.json').write_text(json.dumps(report,indent=2)+'\n')

if __name__=='__main__':main()

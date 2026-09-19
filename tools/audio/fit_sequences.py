#!/usr/bin/env python3
"""Experimental short-window, fixed-timbre pulse paths; no LSDJ recovery claim."""
import json
from pathlib import Path
import numpy as np
from scipy import signal
from scipy.io import wavfile
import fit_opening as base

ROOT=base.ROOT
SR=22050
N=1024
HOP=55


def path_fit(score, penalty=.08):
    # Switching cost independent of interval: fast arpeggio jumps are allowed.
    # Normalize each frame to prevent loud sections dominating this regularizer.
    score=score/np.maximum(score.max(axis=0),1e-9)
    previous=score[:,0].copy()
    back=np.zeros(score.shape,dtype=np.int16)
    for t in range(1,score.shape[1]):
        best=int(np.argmax(previous))
        switch=previous[best]-penalty
        stay=previous>=switch
        back[:,t]=np.where(stay,np.arange(len(previous)),best)
        previous=score[:,t]+np.where(stay,previous,switch)
    path=np.empty(score.shape[1],dtype=int)
    path[-1]=np.argmax(previous)
    for t in range(len(path)-1,0,-1):
        path[t-1]=back[path[t],t]
    return path


def fit(x):
    base.N=N
    definitions,raw=base.dictionary()
    _,times,z=signal.stft(x,SR,nperseg=N,noverlap=N-HOP)
    magnitude=np.abs(z)
    weight=1/np.sqrt(np.maximum(magnitude.mean(axis=1),.0005))
    basis=raw*weight[:,None]
    norm=np.linalg.norm(basis,axis=0)
    basis/=norm
    remainder=magnitude*weight[:,None]
    paths=[]
    prediction=np.zeros_like(magnitude)
    for voice in range(2):
        scores=np.maximum(basis.T@remainder,0)
        choices=[]
        for duty in [.125,.25,.5]:
            indices=np.array([i for i,d in enumerate(definitions) if d[1]==duty])
            local=path_fit(scores[indices])
            chosen=indices[local]
            gain=scores[chosen,np.arange(len(times))]/norm[chosen]
            candidate=raw[:,chosen]*gain
            error=np.sum((remainder-candidate*weight[:,None])**2)
            choices.append((error,chosen,gain,candidate))
        _,chosen,gain,candidate=min(choices,key=lambda c:c[0])
        prediction+=candidate
        remainder-=candidate*weight[:,None]
        paths.append((chosen,gain))
    return definitions,times,z,prediction,paths


def render(definitions,times,paths,length):
    t=np.arange(length)/SR
    outputs=[]
    events=[]
    for voice,(chosen,gain) in enumerate(paths):
        # Midpoint boundaries, one pitch at a time per voice; no per-step retrigger.
        frame=np.minimum(np.searchsorted(times,t+HOP/SR/2,side='right')-1,len(chosen)-1)
        frame=np.maximum(frame,0)
        notes=np.array([definitions[i][0] for i in chosen])
        duty=definitions[chosen[0]][1]
        hz=440*2**((notes[frame]-69)/12)
        phase=np.cumsum(hz)/SR
        wave=np.zeros(length)
        for h in range(1,int((SR/2-1)/hz.min())+1):
            wave+=np.where(h*hz<SR/2,
                2*np.sin(np.pi*h*duty)/(np.pi*h)*np.cos(2*np.pi*h*phase-np.pi*h*duty),0)
        wave*=np.interp(t,times,gain)
        outputs.append(wave)
        boundaries=np.r_[0,np.flatnonzero(np.diff(notes))+1,len(notes)]
        for a,b in zip(boundaries[:-1],boundaries[1:]):
            start=max(0,float(times[a]-HOP/SR/2))
            end=min(length/SR,float(times[b-1]+HOP/SR/2))
            if end>start:
                events.append(dict(voice_hypothesis=voice,start=start,end=end,midi=int(notes[a]),
                                   duty=duty,mean_gain=float(gain[a:b].mean())))
    return outputs,events


def main():
    work=ROOT/'scratchpad/audio/courtesy'
    sr,stereo=wavfile.read(work/'decoded.wav')
    x=signal.resample_poly(stereo[:20*sr].mean(axis=1),SR,sr)
    definitions,times,z,prediction,paths=fit(x)
    outputs,events=render(definitions,times,paths,len(x))
    synth=sum(outputs)
    gain=min(1.,.95/max(abs(synth).max(),1e-9))
    wavfile.write(work/'sequence_resynthesis.wav',SR,(synth*gain).astype(np.float32))
    for i,wave in enumerate(outputs):
        wavfile.write(work/f'sequence_voice_{i}.wav',SR,(wave*gain).astype(np.float32))
    mask=np.minimum(prediction/np.maximum(abs(z),1e-10),1)
    _,estimate=signal.istft(z*mask,SR,nperseg=N,noverlap=N-HOP)
    residual=x-estimate[:len(x)]
    wavfile.write(work/'sequence_mask_residual.wav',SR,residual.astype(np.float32))
    # Compare independent audio syntheses under one common resolution, not masks.
    def error(y):
        _,_,a=signal.stft(x,SR,nperseg=2048,noverlap=1536)
        _,_,b=signal.stft(y,SR,nperseg=2048,noverlap=1536)
        a,b=abs(a),abs(b)
        scale=float(np.sum(a*b)/max(np.sum(b*b),1e-12))
        return float(np.linalg.norm(a-scale*b)/np.linalg.norm(a))
    oldsr,old=wavfile.read(work/'pulse_resynthesis.wav')
    assert oldsr==SR and len(old)==len(x)
    report={'status':'unreviewed experimental paths, not channels or confirmed arpeggios',
            'window_ms':N/SR*1000,'hop_ms':HOP/SR*1000,'preview_gain':float(gain),
            'common_magnitude_error_old':error(old),'common_magnitude_error_new':error(synth),
            'event_count':len(events),'events':events}
    (ROOT/'soundtrack/courtesy.sequence_candidates.json').write_text(json.dumps(report,indent=2)+'\n')
    # Independent rapid-pitch synthetic control; evaluate away from transitions.
    t=np.arange(SR)/SR
    truth=np.array([60,64,67,72])[(t/.04).astype(int)%4]
    phase=np.cumsum(440*2**((truth-69)/12))/SR
    control=.2*signal.square(2*np.pi*phase+.31,duty=.25)
    defs,ct,_,_,cp=fit(control)
    inferred=np.array([defs[i][0] for i in cp[0][0]])
    inside=(ct>.03)&(ct<.97)&(np.abs((ct%.04)-.02)<.005)
    expected=np.array([60,64,67,72])[(ct/.04).astype(int)%4]
    accuracy=float(np.mean(inferred[inside]==expected[inside]))
    assert accuracy>.85,(accuracy,'rapid-pitch control failed')
    assert np.isfinite(synth).all()
    print(json.dumps({k:v for k,v in report.items() if k!='events'},indent=2))
    print('40 ms synthetic pitch-step accuracy:',accuracy)

if __name__=='__main__': main()

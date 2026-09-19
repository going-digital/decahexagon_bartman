#!/usr/bin/env python3
"""Train-only selection of periodic pulse hypotheses, with held-out scoring.
Scores describe one component of a mixture, not transcription accuracy.
"""
import json
import numpy as np
from scipy import signal
from scipy.io import wavfile
import fit_opening as base

SR,N,HOP=22050,512,55


def search(x,train_bounds,test_bounds):
    base.N=N
    definitions,raw=base.dictionary()
    _,t,z=signal.stft(x,SR,nperseg=N,noverlap=N-HOP)
    train=(t>=train_bounds[0])&(t<train_bounds[1])
    test=(t>=test_bounds[0])&(t<test_bounds[1])
    mag=abs(z)
    weight=1/np.sqrt(np.maximum(mag[:,train].mean(axis=1),.0005))
    a=raw*weight[:,None]
    norms=np.linalg.norm(a,axis=0)
    a/=norms
    target=mag*weight[:,None]
    # Maximum nonnegative per-frame gain for each candidate spectrum.
    scores=np.maximum(a.T@target,0)**2
    rows=np.arange(len(t))
    def evaluate(indices,kind,step=0,phase=0,pattern=None):
        return dict(kind=kind,step_seconds=float(step),phase_seconds=float(phase),
                    pattern=pattern,indices=indices,
                    training_score=float(scores[indices,rows][train].mean()),
                    validation_score=float(scores[indices,rows][test].mean()))
    held=int(np.argmax(scores[:,train].mean(axis=1)))
    best={'held':evaluate(np.full(len(t),held),'held',pattern=[held])}
    for kind in ['pitch_cycle','duty_cycle']:
        winner=None
        for length in [2,3,4,6,8]:
            for step in np.arange(.010,.081,.005):
                for shift in [0,.25,.5,.75]:
                    phase=shift*step
                    slot=np.floor((t-phase)/step).astype(int)%length
                    sums=np.stack([scores[:,train&(slot==s)].mean(axis=1)
                                   for s in range(length)],axis=1)
                    groups=([np.arange(d,len(definitions),3) for d in range(3)]
                            if kind=='pitch_cycle' else
                            [np.arange(p,p+3) for p in range(0,len(definitions),3)])
                    for group in groups:
                        pattern=group[np.argmax(sums[group],axis=0)]
                        if len(set(pattern))<2: continue
                        indices=pattern[slot]
                        score=float(scores[indices,rows][train].mean())
                        if winner is None or score>winner['training_score']:
                            winner=evaluate(indices,kind,step,phase,pattern.tolist())
        best[kind]=winner
    energy=float(np.mean(np.sum(target[:,test]**2,axis=0)))
    for model in best.values():
        indices=model.pop('indices')
        model['pattern']=[{'midi':definitions[i][0],'duty':definitions[i][1]} for i in model['pattern']]
        model['heldout_weighted_residual_fraction']=1-model['validation_score']/energy
        model['gain']=np.sqrt(scores[indices,rows])/norms[indices]
    return best,t


def render(model,times,length):
    t=np.arange(length)/SR
    pattern=model['pattern']
    slot=(np.floor((t-model['phase_seconds'])/model['step_seconds']).astype(int)%len(pattern)
          if model['step_seconds'] else np.zeros(len(t),dtype=int))
    hz=440*2**((np.array([p['midi'] for p in pattern])[slot]-69)/12)
    duty=np.array([p['duty'] for p in pattern])[slot]
    phase=np.cumsum(hz)/SR
    wave=np.zeros(len(t))
    for h in range(1,int((SR/2-1)/hz.min())+1):
        wave+=np.where(h*hz<SR/2,2*np.sin(np.pi*h*duty)/(np.pi*h)*np.cos(2*np.pi*h*phase-np.pi*h*duty),0)
    return wave*np.interp(t,times,model['gain'])


def main():
    work=base.WORK
    sr,stereo=wavfile.read(work/'decoded.wav')
    x=signal.resample_poly(stereo[:3*sr].mean(axis=1),SR,sr)
    best,t=search(x,(.2,1.6),(1.7,2.6))
    report={'status':'experimental single-component hypotheses, not recovered instruments',
            'training_seconds':[.2,1.6],'validation_seconds':[1.7,2.6],
            'window_ms':N/SR*1000,'models':{}}
    waves={name:render(model,t,len(x)) for name,model in best.items()}
    gain=min(1.,.95/max(max(abs(w).max() for w in waves.values()),1e-9))
    for name,model in best.items():
        wavfile.write(work/f'repeating_{name}.wav',SR,(waves[name]*gain).astype(np.float32))
        model.pop('gain')
        report['models'][name]=model
    report['preview_gain']=float(gain)
    (base.ROOT/'soundtrack/courtesy.repeating_comparison.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2))
    # Independently generated periodic control, evaluated only on held-out time.
    t=np.arange(SR*3)/SR
    notes=np.array([60,64,67])[(t/.04).astype(int)%3]
    phase=np.cumsum(440*2**((notes-69)/12))/SR
    control=.15*signal.square(2*np.pi*phase+.27,duty=.25)
    models,_=search(control,(.2,1.6),(1.7,2.6))
    assert models['pitch_cycle']['validation_score']>models['held']['validation_score']*1.1
    assert models['pitch_cycle']['validation_score']>models['duty_cycle']['validation_score']*1.1
    print('Independent 40 ms arpeggio control: periodic pitch beats held/duty models on validation')

if __name__=='__main__': main()

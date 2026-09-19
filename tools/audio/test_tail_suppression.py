#!/usr/bin/env python3
"""Bounded delayed-energy suppression experiment, not estimated room inversion."""
import json
import numpy as np
from scipy import signal, ndimage
from scipy.io import wavfile
import fit_opening as base

SR,N,HOP=22050,1024,110


def suppress(x,decay=.16,floor=.7):
    spectra=[]
    for channel in x.T:
        _,times,z=signal.stft(channel,SR,nperseg=N,noverlap=N-HOP)
        spectra.append(z)
    z=np.asarray(spectra)
    power=np.mean(abs(z)**2,axis=0)
    delay=round(.03*SR/HOP)
    history=np.zeros_like(power)
    alpha=np.exp(-HOP/SR/decay)
    for t in range(delay,power.shape[1]):
        history[:,t]=alpha*history[:,t-1]+(1-alpha)*power[:,t-delay]
    # Suppress only bins falling below their recent delayed energy. Sustained
    # sources and overlapping notes still bias this heuristic; no RT60 inferred.
    amount=np.maximum(history-power,0)/np.maximum(history,1e-12)
    gain=np.maximum(floor,1-.5*amount)
    gain=ndimage.gaussian_filter(gain,sigma=(1,.6))
    result=[]
    for channel in z:
        _,y=signal.istft(channel*gain,SR,nperseg=N,noverlap=N-HOP)
        result.append(y[:len(x)])
    return np.asarray(result).T, float(gain.min())


def onset_comparison(x,y):
    def envelope(s):
        _,t,z=signal.stft(s.mean(axis=1),SR,nperseg=512,noverlap=402)
        a=np.maximum(np.diff(np.log1p(abs(z)*1000),axis=1,prepend=np.log1p(abs(z[:,:1])*1000)),0).mean(axis=0)
        return t,a
    t,a=envelope(x); _,b=envelope(y)
    peaks,_=signal.find_peaks(a,prominence=max(a.max()*.08,1e-9),distance=3)
    shifts=[]
    for p in peaks:
        lo=max(0,p-3);hi=min(len(b),p+4)
        shifts.append(abs(t[lo+np.argmax(b[lo:hi])]-t[p]))
    return {'reference_onset_count':len(peaks),
            'median_local_peak_shift_ms':float(np.median(shifts)*1000) if shifts else None,
            'p95_local_peak_shift_ms':float(np.percentile(shifts,95)*1000) if shifts else None}


def fit_error(x):
    base.N=1024
    defs,raw=base.dictionary()
    _,_,z=signal.stft(x.mean(axis=1),SR,nperseg=1024,noverlap=804)
    mag=abs(z); prediction=raw@base.fit(mag,raw)
    return float(np.linalg.norm(mag-prediction)/np.linalg.norm(mag))


def main():
    sr,source=wavfile.read(base.WORK/'decoded.wav')
    x=signal.resample_poly(source[:20*sr],SR,sr,axis=0)
    work=base.WORK/'tail_test';work.mkdir(exist_ok=True)
    wavfile.write(work/'original.wav',SR,x.astype(np.float32))
    report={'status':'unreviewed heuristic; reverb/delay presence and parameters unconfirmed',
            'interval_seconds':[0,20],'sample_rate':SR,'original_fit_error':fit_error(x),'variants':[]}
    for decay,floor in [(.08,.7),(.16,.7),(.30,.7),(.16,.5)]:
        y,minimum=suppress(x,decay,floor)
        name=f'decay{round(decay*1000)}_floor{round(floor*100)}'
        wavfile.write(work/f'{name}.wav',SR,y.astype(np.float32))
        wavfile.write(work/f'{name}_removed.wav',SR,(x-y).astype(np.float32))
        item={'name':name,'history_decay_seconds':decay,'gain_floor':floor,'measured_minimum_gain':minimum,
              'rms_ratio':float(np.linalg.norm(y)/np.linalg.norm(x)),
              'processed_fit_error':fit_error(y),**onset_comparison(x,y)}
        report['variants'].append(item)
        assert y.shape==x.shape and np.isfinite(y).all() and minimum>=floor-1e-6
    # Negative control: dry sustained tone; positive control: known decaying tail.
    t=np.arange(SR*2)/SR
    dry=.2*np.sin(2*np.pi*440*t)
    sustain=np.column_stack([dry,dry])
    clean,_=suppress(sustain)
    sustained_ratio=float(np.linalg.norm(clean[SR//2:SR])/np.linalg.norm(sustain[SR//2:SR]))
    assert sustained_ratio>.98
    dry[t>=.5]=0
    impulse=np.zeros(SR//2);impulse[0]=1
    rng=np.random.default_rng(42)
    impulse[660:]=.03*rng.standard_normal(len(impulse)-660)*np.exp(-np.arange(len(impulse)-660)/(SR*.1))
    wet=signal.fftconvolve(dry,impulse)[:len(dry)]
    pair=np.column_stack([wet,wet]);processed,_=suppress(pair)
    tail=slice(round(.6*SR),round(.9*SR))
    tail_ratio=float(np.linalg.norm(processed[tail])/np.linalg.norm(pair[tail]))
    assert tail_ratio<.95
    report['controls']={'dry_sustained_rms_ratio':sustained_ratio,'known_tail_rms_ratio':tail_ratio}
    (base.ROOT/'soundtrack/courtesy.tail_test.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2))

if __name__=='__main__': main()

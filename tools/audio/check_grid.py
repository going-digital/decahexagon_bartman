#!/usr/bin/env python3
"""Check local onset phase against the proposed constant grid, without claiming beats."""
import json
from pathlib import Path
import numpy as np
from scipy import signal
from scipy.io import wavfile
ROOT=Path(__file__).resolve().parents[2]
results={}
for path in sorted((ROOT/'soundtrack').glob('*.analysis.json')):
    info=json.loads(path.read_text())
    sr, stereo=wavfile.read(ROOT/info['generated_directory']/'decoded.wav')
    x=signal.resample_poly(stereo.mean(axis=1),22050,sr)
    f,t,z=signal.stft(x,22050,nperseg=1024,noverlap=804,boundary=None)
    m=np.log1p(np.abs(z)*1000)
    onset=np.maximum(np.diff(m,axis=1,prepend=m[:,:1]),0).mean(axis=0)
    onset=np.maximum(onset-signal.medfilt(onset,101),0)
    bpm=info['constant_grid_hypothesis']['bpm']
    phase=info['constant_grid_hypothesis']['phase_seconds']
    tracks=[]
    for multiplier in [.5,1,2]:
        period=60/(bpm*multiplier)
        windows=[]
        for start in range(0,int(t[-1])-10,20):
            shifts=np.linspace(-period/2,period/2,129)
            grid=np.arange(phase, t[-1],period)
            grid=grid[(grid>=start+period)&(grid<start+20-period)]
            scores=np.array([np.interp(grid+shift,t,onset).mean() for shift in shifts])
            k=int(np.argmax(scores))
            windows.append({'start_seconds':start,'best_offset_seconds':float(shifts[k]),
                            'peak_to_average_grid_score':float(scores[k]/max(scores.mean(),1e-9))})
        tracks.append({'bpm':bpm*multiplier,'windows':windows})
    results[path.stem.split('.')[0]]={'status':'unreviewed; phase offsets include attack/window bias and metrical ambiguity',
        'half_base_double_grids':tracks}
(ROOT/'soundtrack/grid_diagnostics.json').write_text(json.dumps(results,indent=2)+'\n')
print('Wrote local timing diagnostics for three tracks')

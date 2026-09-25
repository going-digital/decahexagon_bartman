#!/usr/bin/env python3
"""Test simple Courtesy transforms against the owned PC ending assets; make auditions."""
import argparse, hashlib, json, subprocess
from pathlib import Path
import numpy as np
from scipy.signal import correlate, resample_poly
from scipy.io import wavfile
root=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser()
p.add_argument('--music-dir',type=Path,required=True)
a=p.parse_args()
out=root/'scratchpad/audio/ending_courtesy';out.mkdir(parents=True,exist_ok=True)
rate=4000

def decode(path,sr=rate):
    return np.frombuffer(subprocess.check_output(['ffmpeg','-v','error','-i',str(path),'-ac','1','-ar',str(sr),'-f','f32le','-']),dtype='<f4').copy()
def save(name,x,sr):
    wavfile.write(out/(name+'.wav'),sr,(np.clip(x,-1,1)*32767).astype(np.int16))
source=a.music_dir/'music1.dat';courtesy=decode(source)
rows=[];assets={}
for number in [1,4,5]:
    path=a.music_dir/f'music{number}.dat'
    assets[path.name]=hashlib.sha256(path.read_bytes()).hexdigest()
for number in [4,5]:
    target=decode(a.music_dir/f'music{number}.dat')
    matches=[]
    for reverse in [False,True]:
        for up,down in [(2,1),(3,2),(4,3),(5,4),(1,1),(4,5)]:
            x=resample_poly(courtesy[::-1] if reverse else courtesy,up,down)
            cumulative=np.r_[0,np.cumsum(x.astype(np.float64)**2)]
            windows=[]
            for second in [5,30,60]:
                q=target[second*rate:(second+8)*rate]
                c=correlate(x,q,mode='valid',method='fft')
                norm=np.sqrt(np.maximum(cumulative[len(q):]-cumulative[:-len(q)],1e-20)*np.sum(q.astype(np.float64)**2))
                scores=c/np.maximum(norm,1e-20);i=int(np.argmax(abs(scores)))
                windows.append(dict(target_seconds=second,transformed_source_seconds=i/rate,correlation=float(scores[i])))
            matches.append(dict(reverse=reverse,speed=down/up,mean_absolute_correlation=float(np.mean([abs(w['correlation']) for w in windows])),windows=windows))
    matches.sort(key=lambda m:m['mean_absolute_correlation'],reverse=True)
    rows.append(dict(pc_asset=f'music{number}.dat',decoded_seconds=len(target)/rate,candidates=matches))
    save(f'pc_music{number}',decode(a.music_dir/f'music{number}.dat',12000),12000)
bank=root/'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/courtesy_expanded_fibonacci.wav'
x=decode(bank,12000)
save('bank_forward',x,12000);save('bank_half_speed',x,6000);save('bank_reverse',x[::-1],12000)
report=dict(status='Comparison only; no exact reconstruction established',assets_sha256=assets,bank_sha256=hashlib.sha256(bank.read_bytes()).hexdigest(),analysis_rate=rate,results=rows,limitations=['Candidate rates only; not an exhaustive speed search','Lossy coding, edits, effects and pitch-preserving stretch can reduce correlation','Audition candidates are full-bank transforms without ending-specific edits or synchronization'])
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
(root/'docs/ENDING_AUDIO_COMPARISON.json').write_text(json.dumps(report,indent=2)+'\n')
items=[('pc_music4','PC music4 ending asset'),('pc_music5','PC music5 secret-ending asset'),('bank_forward','Current Courtesy bank'),('bank_half_speed','Courtesy bank at half speed (hypothesis)'),('bank_reverse','Courtesy bank reversed (hypothesis)')]
(out/'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>Ending audio comparison</title><style>body{font:18px system-ui;max-width:850px;margin:40px auto;background:#171923;color:#eee}audio{width:100%}</style><h1>Ending audio comparison</h1><p>The bank transforms below are audition hypotheses, not verified reconstructions. Playback levels are not normalized between the PC assets and the boosted Amiga bank.</p>'''+''.join(f'<h2>{label}</h2><audio controls preload="none" src="{name}.wav"></audio>' for name,label in items))
for r in rows:print(r['pc_asset'],r['decoded_seconds'],'best mean absolute correlation',r['candidates'][0]['mean_absolute_correlation'])
print(out/'index.html')

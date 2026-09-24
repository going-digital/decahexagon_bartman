#!/usr/bin/env python3
"""Compare original Otis MP3 with PC music2.dat, without modifying game assets."""
import hashlib,json,subprocess
from pathlib import Path
import numpy as np
from scipy.signal import stft,correlate
from scipy.io import wavfile
ROOT=Path(__file__).resolve().parents[2];out=ROOT/'scratchpad/audio/otis_pc_comparison';out.mkdir(parents=True,exist_ok=True)
p=json.loads((ROOT/'scratchpad/pc_verification/evidence/provenance.json').read_text())
original=ROOT/'assets/Chipzel - Otis [1413220136].mp3';pc=Path(p['binary']).parent.parent/'Resources/data/music/music2.dat'
def decode(path):return np.frombuffer(subprocess.check_output(['ffmpeg','-v','error','-i',str(path),'-ac','1','-ar','8000','-f','f32le','-']),dtype='<f4').copy()
a,b=decode(original),decode(pc)
def feature(x):
 f,t,z=stft(x,8000,nperseg=512,noverlap=352,boundary=None)
 power=np.abs(z)**2;edges=np.geomspace(50,3900,41)
 bands=np.stack([np.log(1e-10+power[max(1,int(np.searchsorted(f,lo))):max(int(np.searchsorted(f,lo))+1,int(np.searchsorted(f,hi)))].mean(axis=0)) for lo,hi in zip(edges[:-1],edges[1:])])
 n=bands.shape[1]//5;return bands[:,:n*5].reshape(40,n,5).mean(axis=2).T
fa,fb=feature(a),feature(b);allf=np.r_[fa,fb];mean=allf.mean(axis=0);std=allf.std(axis=0)
fa=(fa-mean)/std;fb=(fb-mean)/std
fa/=np.maximum(np.linalg.norm(fa,axis=1)[:,None],1e-10);fb/=np.maximum(np.linalg.norm(fb,axis=1)[:,None],1e-10)
windows=np.lib.stride_tricks.sliding_window_view(fa,30,axis=0).transpose(0,2,1)
rows=[]
for second in range(0,141,5):
 q=fb[second*10:second*10+30]
 if len(q)!=30:continue
 scores=np.einsum('ntf,tf->n',windows,q)/30
 at=int(np.argmax(scores));row=dict(pc_seconds=second,original_seconds=at/10,score=float(scores[at]))
 rows.append(row);print(row,flush=True)
report=dict(original=dict(path=str(original.relative_to(ROOT)),sha256=hashlib.sha256(original.read_bytes()).hexdigest(),seconds=len(a)/8000),pc=dict(path=str(pc),sha256=hashlib.sha256(pc.read_bytes()).hexdigest(),seconds=len(b)/8000),method='3-second log-spectral windows, 40 frequency bands, 100 ms steps; repeated passages can be ambiguous',matches=rows)
(out/'matches.json').write_text(json.dumps(report,indent=2)+'\n')
np.savez(out/'features.npz',original=fa,pc=fb)
for name,x in [('original',a),('pc',b)]:wavfile.write(out/f'{name}.wav',8000,np.clip(x*32767,-32768,32767).astype(np.int16))
# Monotonic whole-recording alignment disambiguates repeated phrases.
sim=fb@fa.T;n,m=sim.shape;trace=np.zeros((n+1,m+1),dtype=np.uint8);previous=np.zeros(m+1)
for i in range(1,n+1):
 row=np.full(m+1,-1e8);row[0]=-i*.6
 for j in range(1,m+1):
  choices=(previous[j-1]+sim[i-1,j-1]-.35,previous[j]-.6,row[j-1]-.6)
  k=int(np.argmax(choices));row[j]=choices[k];trace[i,j]=k
 previous=row
j=int(np.argmax(previous));i=n;path=[]
while i and j:
 k=trace[i,j]
 if k==0:path.append((i-1,j-1));i-=1;j-=1
 elif k==1:i-=1
 else:j-=1
path=np.array(path[::-1]);offsets=(path[:,1]-path[:,0])/10
report['whole_sequence_alignment']=dict(resolution_seconds=.1,matched_frames=len(path),pc_frames=n,offset_min=float(offsets.min()),offset_max=float(offsets.max()),offset_median=float(np.median(offsets)),interpretation='Monotonic spectral alignment supports one leading trim, not internal rearrangement or a material tempo change.')
# Refine the offset using amplitude envelopes at unchanged playback speed.
def envelope(x):
 x=x[:len(x)//80*80].reshape(-1,80);return np.sqrt(np.mean(x*x,axis=1))
ea,eb=envelope(a),envelope(b);ta=(np.arange(len(ea))+.5)/100;tb=(np.arange(len(eb))+.5)/100
refinements=[]
for begin in [5,30,60,90,120]:
 use=(tb>=begin)&(tb<begin+20);q=eb[use];q=q-q.mean();scores=[]
 for shift in np.arange(6.5,6.901,.001):
  x=np.interp(tb[use]+shift,ta,ea);x-=x.mean();scores.append((float(np.dot(q,x)/(np.linalg.norm(q)*np.linalg.norm(x))),float(shift)))
 score,shift=max(scores);refinements.append(dict(pc_start=begin,offset_seconds=shift,correlation=score))
report['envelope_offset_refinement']=refinements
shift=float(np.median([x['offset_seconds'] for x in refinements]));report['estimated_trim']=dict(start_seconds=shift,end_seconds=len(a)/8000-len(b)/8000-shift)
report['limitations']=['Spectral/envelope alignment establishes structure, not sample identity.','Low waveform correlation prevents claiming the PC file is merely a byte-equivalent trim/re-encode.','Offsets are approximate; exact cue synchronization should use the PC recording itself.']
for name,x in [('pc_opening',b[:15*8000]),('original_opening',a[:15*8000]),('original_aligned',a[round(shift*8000):round((shift+15)*8000)]),('pc_ending',b[-15*8000:]),('original_ending',a[-19*8000:])]:
 wavfile.write(out/f'{name}.wav',8000,np.clip(x*32767,-32768,32767).astype(np.int16))
(out/'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>Otis: original versus PC</title><style>body{font:18px system-ui;max-width:850px;margin:40px auto;background:#171923;color:#eee}audio{width:100%}</style><h1>Otis: original versus PC</h1><p>Timing comparison, decoded to 8 kHz mono; no gain normalization. Original: 156.444 s. PC: 145.883 s. Whole-sequence spectral alignment supports roughly 6.7 s removed from the start and 3.9 s from the end. These previews are for structure, not full-band fidelity.</p>'''+''.join(f'<p>{title}</p><audio controls preload="none" src="{name}.wav"></audio>' for name,title in [('original_opening','Original opening'),('pc_opening','PC opening'),('original_aligned','Original starting approximately 6.68 seconds in'),('pc_ending','PC ending (last 15 seconds)'),('original_ending','Original ending (last 19 seconds)')]))
(ROOT/'soundtrack/otis_pc_comparison.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['whole_sequence_alignment']);print(refinements);print(report['estimated_trim'])

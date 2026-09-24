#!/usr/bin/env python3
"""Verify soundtrack identity/timeline against installed PC audio and extract cues."""
import hashlib,json,struct,subprocess
from pathlib import Path
import numpy as np
from scipy.signal import correlate,resample_poly
from scipy.io import wavfile
ROOT=Path(__file__).resolve().parents[2]
provenance=json.loads((ROOT/'scratchpad/pc_verification/evidence/provenance.json').read_text())
binary=Path(provenance['binary']);data=binary.read_bytes()
assert hashlib.sha256(data).hexdigest()==provenance['sha256']
work=ROOT/'scratchpad/audio/pc_tracks';work.mkdir(parents=True,exist_ok=True)
pcs={};hashes={}
for track in (1,2,3):
 source=binary.parent.parent/f'Resources/data/music/music{track}.dat'
 hashes[track]=hashlib.sha256(source.read_bytes()).hexdigest()
 raw=subprocess.check_output(['ffmpeg','-v','error','-i',str(source),'-ac','1','-ar','2000','-f','f32le','-'])
 pcs[track]=np.frombuffer(raw,dtype='<f4').astype(np.float64)
rows=[]
for name in ['courtesy','focus','otis']:
 rate,audio=wavfile.read(ROOT/f'scratchpad/audio/{name}/rate_test/12000/reference.wav');assert rate==12000
 reference=resample_poly(audio.astype(np.float64)/32768,1,6)
 matches=[]
 for second in [10,30,60,90,120]:
  sample=reference[second*2000:(second+3)*2000].copy();sample-=sample.mean();norm=np.linalg.norm(sample)
  candidates=[]
  for track,pc in pcs.items():
   n=len(sample);sums=np.r_[0,np.cumsum(pc)];squares=np.r_[0,np.cumsum(pc*pc)]
   energy=squares[n:]-squares[:-n]-(sums[n:]-sums[:-n])**2/n
   scores=correlate(pc,sample,mode='valid',method='fft')/(norm*np.sqrt(np.maximum(energy,1e-20)))
   at=int(np.argmax(scores));candidates.append(dict(pc_track=track,pc_seconds=at/2000,correlation=float(scores[at])))
  best=max(candidates,key=lambda r:r['correlation']);best['source_seconds']=second;matches.append(best)
 confirmed=all(m['correlation']>=0.8 for m in matches) and len({m['pc_track'] for m in matches})==1
 offsets=[m['pc_seconds']-m['source_seconds'] for m in matches]
 row=dict(soundtrack=name,matches=matches,waveform_identity_confirmed=confirmed,
          fixed_timeline_offset_seconds=sum(offsets)/len(offsets) if confirmed and max(offsets)-min(offsets)<0.002 else None)
 if name=='otis':
  # Envelope evidence can identify repeated musical passages, but cannot by
  # itself establish a unique sample/cue timeline or an exact edit boundary.
  def envelope(x):
   x=x[:len(x)//40*40].reshape(-1,40)
   return np.sqrt(np.mean(x*x,axis=1))
  ep=envelope(pcs[2]);es=envelope(reference);observations=[]
  for second in [10,30,60,90,120]:
   q=es[second*50:(second+10)*50].copy();q-=q.mean();n=len(q)
   sums=np.r_[0,np.cumsum(ep)];sq=np.r_[0,np.cumsum(ep*ep)]
   energy=sq[n:]-sq[:-n]-(sums[n:]-sums[:-n])**2/n
   scores=correlate(ep,q,mode='valid')/(np.linalg.norm(q)*np.sqrt(np.maximum(energy,1e-20)))
   at=int(np.argmax(scores));observations.append(dict(source_seconds=second,pc_seconds=at/50,correlation=float(scores[at])))
  row.update(source_duration_seconds=len(reference)/2000,pc_track2_duration_seconds=len(pcs[2])/2000,envelope_matches=observations,cue_alignment_verified=False,note='Repeated passages and different durations: do not infer a single cue offset from envelope matches.')
 rows.append(row);print(row,flush=True)
# Verify literal addresses from the PC RIP-relative LEAs, not a text dump.
cues=[]
for track,instruction,length in [(1,0x5d3fb,11441),(2,0x5d42b,8830),(3,0x5d443,9734)]:
 assert data[instruction:instruction+3]==b'\x48\x8d\x35'
 offset=instruction+7+struct.unpack_from('<i',data,instruction+3)[0]
 values=[int(v) for v in data[offset:data.index(b'\0',offset)].split(b',') if v.strip()]
 assert len(values)==length
 if track==2:values=[int(v*0.6) for v in values] # matches double multiply and truncation in PC
 assert min(values)>=0 and max(values)<=255
 path=ROOT/f'assets/music{track}.cues'
 if track==1:assert path.read_bytes()==bytes(values)
 else:path.write_bytes(bytes(values))
 cues.append(dict(track_id=track,literal_address=hex(0x100000000+offset),entries=length,sha256=hashlib.sha256(bytes(values)).hexdigest(),scale=0.6 if track==2 else 1))
report=dict(source_sha256=provenance['sha256'],pc_audio_sha256=hashes,method='Five independent 3-second windows; normalized waveform correlation at 2 kHz across all three PC recordings',tracks=rows,cues=cues)
(ROOT/'soundtrack/pc_track_verification.json').write_text(json.dumps(report,indent=2)+'\n')

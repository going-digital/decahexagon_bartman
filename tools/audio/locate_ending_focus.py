#!/usr/bin/env python3
"""Locate pitch-preserved ending passages using time-scaled spectral fingerprints."""
import argparse, hashlib, html, json, subprocess
from pathlib import Path
import numpy as np
from scipy import signal

ROOT = Path(__file__).resolve().parents[2]
RATE, HOP = 12000, 600

def decode(path):
    return np.frombuffer(subprocess.check_output(['ffmpeg', '-v', 'error', '-i', str(path), '-ac', '1', '-ar', str(RATE), '-f', 'f32le', '-']), dtype='<f4').copy()

def features(x):
    f, t, z = signal.stft(x, RATE, nperseg=4096, noverlap=4096-HOP, boundary='zeros')
    edges = np.geomspace(70, 5500, 97)
    power = abs(z)**2
    b = np.array([power[(f >= a) & (f < b)].mean(axis=0) for a, b in zip(edges[:-1], edges[1:])])
    b = np.log(np.maximum(b, 1e-10))
    # Remove recording EQ and overall loudness, retaining changing spectral shape.
    b -= b.mean(axis=1, keepdims=True)
    b -= b.mean(axis=0, keepdims=True)
    b /= np.maximum(np.linalg.norm(b, axis=0, keepdims=True), 1e-9)
    return t, b

def scan(source, target, stretch):
    # Compress the target feature timeline, not its audio/pitch.
    positions = np.arange(0, target.shape[1]-1, stretch)
    q = np.array([np.interp(positions, np.arange(len(row)), row) for row in target])
    q /= np.maximum(np.linalg.norm(q, axis=0, keepdims=True), 1e-9)
    c = sum(signal.correlate(row, query, mode='valid', method='fft') for row, query in zip(source, q))/q.shape[1]
    return c

def main():
    p=argparse.ArgumentParser(); p.add_argument('--music-dir', type=Path, required=True); a=p.parse_args()
    out=ROOT/'scratchpad/audio/ending_focus'; out.mkdir(parents=True, exist_ok=True)
    paths=[a.music_dir/f'music{n}.dat' for n in (3,4,5)]
    source=decode(paths[0]); _, sf=features(source)
    report={'method':'96 log spectral bands, per-recording EQ/loudness removal, time-scaled cosine matching; similarity is not proof', 'sha256':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}, 'results':[]}
    for path in paths[1:]:
        target=decode(path); _, tf=features(target); candidates=[]
        for reverse in (False, True):
            s=sf[:, ::-1] if reverse else sf
            for stretch in np.arange(6, 10.001, .05):
                c=scan(s, tf, stretch)
                peaks,_=signal.find_peaks(c, distance=100)
                peaks=sorted(set([0,len(c)-1,*peaks]), key=lambda i:c[i], reverse=True)[:3]
                for i in peaks:
                    candidates.append(dict(stretch=round(float(stretch),2), reverse=reverse, source_start_seconds=round(float(i*HOP/RATE),3), similarity=float(c[i])))
        candidates.sort(key=lambda r:r['similarity'], reverse=True)
        best=candidates[0]
        # Independent thirds should agree on source start after subtracting target offset / stretch.
        thirds=[]
        for j in range(3):
            begin=j*tf.shape[1]//3; end=(j+1)*tf.shape[1]//3
            c=scan(sf[:,::-1] if best['reverse'] else sf, tf[:,begin:end], best['stretch']); i=int(c.argmax())
            thirds.append(dict(target_start_seconds=begin*HOP/RATE, inferred_source_origin_seconds=i*HOP/RATE-begin*HOP/RATE/best['stretch'],similarity=float(c[i])))
        result=dict(asset=path.name, duration_seconds=len(target)/RATE, best=best, independent_thirds=thirds, candidates=candidates[:20]);report['results'].append(result)
        print(json.dumps(result, indent=2), flush=True)
    (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    sections=[]
    for result, path in zip(report['results'], paths[1:]):
        best=result['best']
        if best['similarity'] < .4 or best['reverse']:
            continue  # Do not present weak matches as identified passages.
        start=best['source_start_seconds']; duration=result['duration_seconds']/best['stretch']
        prefix=path.stem
        bank=ROOT/'scratchpad/audio/codec_audition_budget_250_preboost_pc_tracks/focus_expanded_fibonacci.wav'
        inputs=[('reference',path,0,result['duration_seconds'],False,'PC ending reference'),
                ('source',paths[0],start,duration,False,'Focus source passage at original speed'),
                ('stretch',paths[0],start,duration,True,'PC Focus passage stretched 8× (FFmpeg atempo)'),
                ('bank_stretch',bank,start,duration,True,'Shipped Focus reconstruction stretched 8× (FFmpeg atempo)')]
        items=[]
        for name, src, offset, length, stretched, label in inputs:
            if stretched and best['stretch'] != 8:
                continue
            filters=f'atrim=start={offset}:duration={length},asetpts=PTS-STARTPTS'
            if stretched: filters+=',atempo=0.5,atempo=0.5,atempo=0.5'
            output=f'{prefix}_{name}.wav'
            subprocess.run(['ffmpeg','-v','error','-y','-i',str(src),'-af',filters,'-ac','1','-ar',str(RATE),'-c:a','pcm_s16le',str(out/output)],check=True)
            items.append(f'<h3>{html.escape(label)}</h3><audio controls preload="none" src="{output}"></audio>')
        sections.append(f'<h2>{prefix}: Focus {start:.2f}–{start+duration:.2f} seconds</h2>'+''.join(items))
    (out/'index.html').write_text('''<!doctype html><meta charset="utf-8"><title>Focus ending alignment</title>
<style>body{font:18px system-ui;max-width:900px;margin:40px auto;padding:16px;background:#171923;color:#eee}audio{width:100%}</style>
<h1>Focus ending alignment</h1><p>Normal ending: strong spectral match to Focus from about 24.35 seconds, stretched to eight times the duration. This identifies a passage, not the original processing algorithm. Secret ending: no convincing match in the 6–10× forward/reverse search.</p>
<p>Stretch previews use three successive half-tempo passes of FFmpeg atempo; they are listening references, not the proposed Amiga implementation or an exact recreation of the PC effect. No independent loudness normalization is applied; the Amiga bank has its existing gain and codec processing. Minor duration differences can result from the stretch filter.</p>'''+''.join(sections))
    print(out/'index.html')

if __name__=='__main__': main()

#!/usr/bin/env python3
"""Offline cross-check of the accepted MP3's time origin against PC Courtesy.
Requires numpy/scipy and ffmpeg; does not alter either soundtrack or beat map.
"""
import argparse,json,subprocess
from pathlib import Path
import numpy as np
from scipy.signal import correlate
ROOT=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser();p.add_argument('pc_music1',type=Path);args=p.parse_args()
def decode(path):
    data=subprocess.check_output(['ffmpeg','-v','error','-i',str(path),'-ac','1','-ar','2000','-f','f32le','-'])
    return np.frombuffer(data,dtype='<f4')
x=decode(next((ROOT/'assets').glob('*Courtesy*.mp3')));y=decode(args.pc_music1)
rows=[]
for t in (10,30,90,170):
    window=x[t*2000:(t+5)*2000]
    c=correlate(y,window,mode='valid',method='fft');i=int(np.argmax(np.abs(c)))
    rho=float(c[i]/np.sqrt(np.sum(window**2)*np.sum(y[i:i+len(window)]**2)))
    offset=i/2000-t
    assert rho>.9 and abs(offset+.051)<.001,(t,offset,rho)
    rows.append(dict(mp3_seconds=t,pc_seconds=i/2000,offset_seconds=offset,correlation=rho))
print(json.dumps(rows,indent=2))

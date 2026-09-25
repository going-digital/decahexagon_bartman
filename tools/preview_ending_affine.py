#!/usr/bin/env python3
"""Generate an offline visual comparison from the actual portable C transforms."""
from pathlib import Path
import json,subprocess
root=Path(__file__).resolve().parents[1]
out=root/'scratchpad/ending_visual';out.mkdir(exist_ok=True)
subprocess.run(['cc','-std=c99','-O2',str(root/'tools/ending_visual_probe.c'),str(root/'pc_ending_affine.c'),str(root/'pc_ending_projection.c'),'-lm','-o',str(out/'probe')],check=True)
frames=[json.loads(s) for s in subprocess.check_output([str(out/'probe')],text=True).splitlines()]
assert len(frames)==108
page='''<!doctype html><meta charset="utf-8"><title>Ending camera comparison</title>
<style>body{background:#181818;color:#eee;font:16px system-ui;margin:32px}section{display:flex;gap:24px;flex-wrap:wrap}canvas{width:640px;max-width:100%;background:#000;image-rendering:pixelated}label{margin-right:24px}p{max-width:900px}</style>
<h1>Ending camera comparison</h1><p>Synthetic planar wall scene, projected by the actual C implementations. This is not a captured PC ending or an Amiga blitter render. Canvas uses even-odd fill; sprite, palette, timing and final-scene behavior are not represented.</p>
<label>Tilt <select id="tilt"><option>0</option><option>20</option><option>30</option></select></label>
<label>Otis <select id="otis"><option>-20</option><option selected>0</option><option>20</option></select></label>
<label>Rotation <input id="rotation" type="range" min="0" max="11" value="0"></label>
<section><div><h2>Affine approximation</h2><canvas id="a" width="320" height="200"></canvas></div><div><h2>Perspective reference</h2><canvas id="p" width="320" height="200"></canvas></div></section>
<p id="status"></p><script>const frames=DATA;
const tilt=document.querySelector('#tilt'),otis=document.querySelector('#otis'),rotation=document.querySelector('#rotation');
function draw(){const f=frames.find(f=>f.tilt==tilt.value&&f.otis==otis.value&&f.rotation==rotation.value*5);
for(const [id,offset] of [['a',0],['p',2]]){const c=document.getElementById(id).getContext('2d');c.clearRect(0,0,320,200);c.beginPath();for(const poly of f.polygons){poly.forEach((v,i)=>i?c.lineTo(v[offset],v[offset+1]):c.moveTo(v[offset],v[offset+1]));c.closePath();}c.fillStyle='#f6ad55';c.fill('evenodd');}
document.querySelector('#status').textContent=`Tilt ${f.tilt}°, Otis ${f.otis}°, field rotation ${f.rotation}°. Both use centre depth 600 and focal scale 300; affine holds depth constant across the plane.`;}
for(const e of [tilt,otis,rotation])e.addEventListener('input',draw);draw();</script>'''
(out/'index.html').write_text(page.replace('DATA',json.dumps(frames)))
print(out/'index.html')

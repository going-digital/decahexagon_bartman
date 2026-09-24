#!/usr/bin/env python3
"""Execute the built Musashi probe for production streams and DEFLATE controls."""
import hashlib,json,random,subprocess,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
paths=list(out.glob('*.deflate'))
rng=random.Random(42)
for name,raw,level in [('stored',rng.randbytes(65537),0),('random',rng.randbytes(65537),9),('repeat',b'ABCD'*131072,9),('mixed',b'A'*200000+rng.randbytes(200000),9),('tiny',b'x',9)]:
 c=zlib.compressobj(level,wbits=-15);p=out/f'control_{name}.deflate';p.write_bytes(c.compress(raw)+c.flush());paths.append(p)
# Explicit fixed-Huffman maximum-distance match, independent of compressor heuristics.
bits=[]
def emit(value,n):
 bits.extend((value>>i)&1 for i in range(n))
def symbol(s):
 if s<=143: value,n=0x30+s,8
 elif s<=255: value,n=0x190+s-144,9
 elif s<=279: value,n=s-256,7
 else: value,n=0xc0+s-280,8
 emit(int(f'{value:0{n}b}'[::-1],2),n)
emit(3,3) # final block, fixed Huffman
prefix=random.Random(73).randbytes(32768)
for byte in prefix:symbol(byte)
symbol(285) # length 258
emit(int(f'{29:05b}'[::-1],2),5);emit(8191,13) # distance 32768
symbol(256)
bits.extend([0]*((-len(bits))%8))
p=out/'control_distance32768.deflate'
p.write_bytes(bytes(sum(bits[i+j]<<j for j in range(8)) for i in range(0,len(bits),8)))
assert zlib.decompress(p.read_bytes(),-15)==prefix+prefix[:258]
paths.append(p)
rows=[]
for p in sorted(set(paths)):
 raw=zlib.decompress(p.read_bytes(),-15);expected=p.with_suffix('.expected');expected.write_bytes(raw)
 result=subprocess.run([str(out/'inflate_probe'),str(out/'inflate.bin'),str(p),str(expected)],text=True,capture_output=True)
 if result.returncode:
  rows.append({'name':p.name,'passed':False,'error':result.stderr.strip()});print(rows[-1],flush=True);continue
 row=json.loads(result.stdout);row['name']=p.name;row['sha256']=hashlib.sha256(p.read_bytes()).hexdigest();row['cpu_seconds_pal_lower_bound']=row['cycles']/7093790
 rows.append(row);print(row,flush=True)
(root/'docs/TRACKLOADER_INFLATE_RESULTS.json').write_text(json.dumps({'status':'Actual 68000 core, disjoint measurement and overlapping replay; no chipset contention/timing','core_binary_sha256':hashlib.sha256((out/'inflate.bin').read_bytes()).hexdigest(),'trials':rows},indent=2)+'\n')
if any(row.get('passed') is False for row in rows):
 raise SystemExit('Inflate validation failed; see recorded trial errors')

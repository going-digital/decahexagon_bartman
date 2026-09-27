#!/usr/bin/env python3
from pathlib import Path
import struct,json,collections,bisect
import argparse,subprocess,hashlib
parser=argparse.ArgumentParser(description="Summarise Copperline native CPU/blitter samples against the matching game ELF")
parser.add_argument('profile',type=Path);parser.add_argument('--elf',type=Path,required=True);parser.add_argument('--out',type=Path,required=True)
args=parser.parse_args();p=args.profile
profile_info=json.loads((p/'profile.json').read_text())
assert profile_info['machine']['video_standard']=='Pal', 'Timing conversion currently supports PAL captures only'
# Frame timestamps are sampled at host polling boundaries. Integrate the
# complete-frame CCK lengths instead (PAL CPU 7,093,790 Hz / 2).
cck_hz=3546895
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt'))
text=subprocess.check_output([str(sdk/'m68k-amiga-elf/bin/nm'),'-n',str(args.elf)],text=True)
import tempfile
with tempfile.TemporaryDirectory() as tmp:
 binary=Path(tmp)/'game.bin'
 subprocess.run([str(sdk/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(args.elf),str(binary)],check=True)
 address=int(next(l.split()[0] for l in text.splitlines() if l.endswith(' pc_turn')),16)
 code=binary.read_bytes();code_bytes=len(code)
 signature=code[address:address+32]
 ram=(p/'chip-ram.bin').read_bytes();found=ram.find(signature)
 assert found>=0 and ram.find(signature,found+1)<0,'ELF does not uniquely match Chip RAM'
 base=found-address

symbols=[]
for l in text.splitlines():
 a=l.split()
 if len(a)==3 and a[1].lower()=='t':symbols.append((int(a[0],16)+base,a[2]))
addresses=[s[0] for s in symbols]
counts=collections.Counter();waits=collections.Counter();owners=collections.Counter();blits=collections.Counter();busy=0;total=0;raw=collections.Counter();times=[];frames=0;entries=collections.Counter();seen=set()
for line in (p/'profile.jsonl').read_text().splitlines():
 r=json.loads(line)
 if r['partial']:continue
 times.append(r['seconds'])
 frames+=1;total+=r['cck_length'];busy+=r['blitter']['busy_cck'];owners.update(r['owner_cck']);waits.update(r['cpu']['wait_by'])
 for b in r['blits']:
  identity=(b['start_frame'],tuple(b['start']),b['dpt'])
  if identity in seen:continue
  seen.add(identity)
  blits['line' if b['line_mode'] else 'fill' if b['fill_mode'] else 'clear' if b['minterm']=='0x00' else b['minterm']]+=1
 data=struct.unpack('<'+str((p/r['samples']).stat().st_size//4)+'I',(p/r['samples']).read_bytes());meta=struct.unpack('<'+str((p/r['samples_meta']).stat().st_size//4)+'I',(p/r['samples_meta']).read_bytes());i=0
 for j in range(meta[2]):
  pc=data[i];entries[pc]+=1
  while data[i]<0xffff0000:i+=1
  i+=18
  clock=meta[3+j*5];raw[pc]+=clock
  index=bisect.bisect_right(addresses,pc)-1
  name=symbols[index][1] if base<=pc<base+code_bytes and index>=0 else 'outside game'
  counts[name]+=clock
print('frames',frames,'total_cck',total,'blitter busy %',busy/total*100,'blits',blits)
print('entry calls',[(name,entries[addr]) for addr,name in symbols if name in ('render_game','blit_cls','game_update')])
print('bus owners',owners,'CPU waits',waits)
for n,c in counts.most_common(22):print(n,round(100*c/total,2),c)
args.out.write_text(json.dumps(dict(elf_sha256=hashlib.sha256(args.elf.read_bytes()).hexdigest(),runtime_base=base,function_entries={name:entries[addr] for addr,name in symbols if name in ('pc_render_angle','pc_morph_arc','project_state','game_update')},render_calls=entries[dict((name,addr) for addr,name in symbols)['render_game']],duration_seconds=total/cck_hz,frames=frames,total_cck=total,busy_cck=busy,blits=blits,owners=owners,waits=waits,functions=counts),indent=2))

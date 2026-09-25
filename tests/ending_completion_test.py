#!/usr/bin/env python3
"""Compare live SFX completion classification with original PC winlevel results."""
from pathlib import Path
import subprocess
import tempfile
root = Path(__file__).resolve().parents[1]
rows = []
for line in (root/'tests/fixtures/pc_ending_entry_native.txt').read_text().splitlines():
    if line.startswith('completion '):
        rows.append(tuple(map(int, line.split()[1:])))
assert len(rows) == 12
cases = ','.join('{%d,%d,%d}' % row for row in rows)
source = '''#include <assert.h>
#include "pc_sfx.h"
static const unsigned cases[][3]={CASES};
int main(void){
 for(unsigned i=0;i<12;i++){
  PcSfx s={0};pc_sfx_begin(&s,0,cases[i][1],cases[i][0]);
  for(unsigned tick=1;tick<=3600;tick++)pc_sfx_live(&s,tick);
  assert(s.completion==0);
  pc_sfx_live(&s,3601);assert(s.completion==cases[i][2]);
  pc_sfx_live(&s,3602);assert(s.completion==cases[i][2]);
 }
 return 0;
}
'''.replace('CASES', cases)
with tempfile.TemporaryDirectory() as directory:
    p = Path(directory)
    (p/'test.c').write_text(source)
    subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),
                    str(p/'test.c'),str(root/'pc_sfx.c'),'-o',str(p/'test')],check=True)
    subprocess.run([str(p/'test')],check=True)
print('All 12 completion classifications match PC entry fixtures; 60-second boundaries pass')

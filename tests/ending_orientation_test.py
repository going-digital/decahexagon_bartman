#!/usr/bin/env python3
"""Compare the production angle conversion with captured PC ending vertices."""
from pathlib import Path
import subprocess,tempfile,math
root=Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory() as d:
 p=Path(d)
 p.joinpath('test.c').write_text('''#include <stdio.h>
#include "pc_ending.h"
int main(void){for(unsigned a=0;a<720;++a){
 unsigned screen=pc_ending_render_angle(a);
 if(pc_ending_angle_from_render(screen)!=a)return 1;
 printf("%u\\n",screen);
}return 0;}
''')
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),str(p/'test.c'),'-o',str(p/'test')],check=True)
 angles=list(map(int,subprocess.check_output([str(p/'test')],text=True).split()))
for row in (root/'tests/fixtures/pc_ending_orientation_native.txt').read_text().splitlines():
 angle,slot,x,y=map(float,row.split())
 screen=angles[int(angle)*2]*2*math.pi/65536-slot*math.pi/3
 assert abs(100*math.cos(screen)-x)<0.011
 assert abs(100*math.sin(screen)-y)<0.011
print('Ending orientation: 30 native PC vertices and all 720 half-degree round trips pass')

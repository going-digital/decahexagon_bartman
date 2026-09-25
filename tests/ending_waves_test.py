#!/usr/bin/env python3
"""Run portable selector against the captured PC wave choices and draw counts."""
from pathlib import Path
import gzip, subprocess, tempfile
root=Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory() as directory:
    p=Path(directory)
    (p/'fixture.txt').write_bytes(gzip.decompress((root/'tests/fixtures/pc_ending_waves_native.txt.gz').read_bytes()))
    subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror',str(root/'pc_ending_waves.c'),str(root/'pc_ending.c'),str(root/'pc_world.c'),str(root/'pc_core.c'),str(root/'pc_waves.c'),str(root/'tests/ending_waves_test.c'),'-o',str(p/'test')],check=True)
    subprocess.run([str(p/'test'),str(p/'fixture.txt')],check=True)

#!/usr/bin/env python3
"""Fail a disabled build if the assist object or symbols reached the link."""
import subprocess
import sys
from pathlib import Path

elf, map_file = sys.argv[1:]
symbols = subprocess.run(['m68k-amiga-elf-objdump', '-t', elf],
                         check=True, capture_output=True, text=True).stdout
for line in symbols.splitlines():
    if 'cheat_' in line or 'k_cheat' in line:
        raise SystemExit('Release cheat audit failed: ' + line)
if 'cheat.o' in Path(map_file).read_text():
    raise SystemExit('Release cheat audit failed: cheat.o was linked')
print('Release cheat audit passed: no assist object or symbols')

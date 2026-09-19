#!/usr/bin/env python3
"""Execute palette reference on macOS; emit fixture text on stdout.
Requires the owned, matching PC executable and x86-64 execution support.
Usage: python3 tools/capture_pc_palette.py [path/to/SuperHexagon]
"""
from pathlib import Path
import hashlib
import subprocess
import sys
root=Path(__file__).resolve().parents[1]
binary=Path(sys.argv[1]) if len(sys.argv)>1 else Path.home()/'Library/Application Support/Steam/steamapps/common/Super Hexagon/Super Hexagon.app/Contents/MacOS/SuperHexagon'
expected='91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88'
if hashlib.sha256(binary.read_bytes()).hexdigest()!=expected:
    raise SystemExit('Executable differs: derive addresses again before running this probe.')
(root/'out').mkdir(exist_ok=True)
exe=root/'out/pc_palette_probe'
subprocess.run(['clang','-arch','x86_64','-O0',str(root/'tools/probe_pc_palette.c'),'-o',str(exe)],check=True)
subprocess.run([str(exe),str(binary)],check=True)

"""Current soundtrack source selection; legacy source assets remain untouched."""
import json
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
def pc_source(track):
    path=ROOT/'soundtrack'/f'{track}.pc_source.json'
    return json.loads(path.read_text()) if path.exists() else None

def pc_otis():
    return pc_source("otis")

def work_directory(track):
    return ROOT/'scratchpad/audio'/(track+'_pc' if pc_source(track) else track)

def compression_grid(track):
    source=pc_source(track)
    return source['compression_grid'] if source else json.loads((ROOT/'soundtrack'/f'{track}.analysis.json').read_text())['constant_grid_hypothesis']

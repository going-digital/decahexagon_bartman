#!/bin/sh
# Run a private A500 verification instance; do not alter the user's launcher config.
set -eu
name=${1:-pal}
case "$name" in pal|ntsc|pal512) ;; *) echo 'usage: tools/run_fsuae.sh pal|ntsc|pal512' >&2; exit 2 ;; esac
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
run="$root/scratchpad/fsuae/$name"
mkdir -p "$run"
rom=${FSUAE_KICKSTART:-"$HOME/Documents/FS-UAE/Kickstarts/Kickstart v1.3 r34.005 (1987-12)(Commodore)(A500-A1000-A2000-CDTV)[!].rom"}
emulator=${FSUAE_BIN:-/Applications/FS-UAE.app/Contents/MacOS/fs-uae}
slow=512
ntsc=0
[ "$name" != ntsc ] || ntsc=1
[ "$name" != pal512 ] || slow=0
cp "$root/out/hexagon.adf" "$run/hexagon.adf"
cat > "$run/check.fs-uae" <<CONFIG
[fs-uae]
amiga_model = A500
chip_memory = 512
slow_memory = $slow
fast_memory = 0
ntsc_mode = $ntsc
kickstart_file = $rom
floppy_drive_0 = $run/hexagon.adf
floppy_drive_0_sounds = 0
fullscreen = 0
window_width = 800
window_height = 600
automatic_input_grab = 0
initial_input_grab = 0
base_dir = $run
screenshots_output_dir = $run
screenshots_output_mask = 3
CONFIG
exec "$emulator" "$run/check.fs-uae"

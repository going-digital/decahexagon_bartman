#!/bin/sh
# Prefer an installed toolchain; otherwise use the local Bartman VS Code SDK.
set -eu
if ! command -v m68k-amiga-elf-gcc >/dev/null 2>&1; then
    for sdk in "$HOME"/.vscode/extensions/bartmanabyss.amiga-debug-*/bin/darwin; do
        if [ -x "$sdk/opt/bin/m68k-amiga-elf-gcc" ]; then
            PATH="$sdk/opt/bin:$sdk:$PATH"
            export PATH
            break
        fi
    done
fi
exec make "$@"

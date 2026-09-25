Hexagon for Amiga

TEST LEVEL SHORTCUTS (from the title/level-selection screen)
  F1 Hexagon             F4 Hyper Hexagon
  F2 Hexagoner           F5 Hyper Hexagoner
  F3 Hexagonest          F6 Hyper Hexagonest
  F7 Hexagonest bonus entry (120-second test snapshot)
These bypass level locks without unlocking levels. Test runs and their retries
do not update records or completion flags. Escape returns to selection; starting
normally with Space/Return resumes normal score saving. F7 starts a representative
bonus-entry snapshot, not a replay of the preceding two minutes.

ADF/hexagon.adf
  Bootable floppy edition. Write it to an Amiga floppy or mount it in an emulator.
  The distributed image has blank save slots. Use a writable working copy to save
  progress, and retain that copy when installing a newer distribution.

WHDLoad/Hexagon/
  Copy this entire directory to your Amiga hard disk. WHDLoad 20 is required.
  Launch Hexagon-1, -2 or -3 from Workbench for one, two or three decoded
  soundtrack cache slots. See the included README.md for memory requirements,
  Shell launch commands, save handling and validation limits.
  Keep existing data/save-a and data/save-b when updating an installation.

Both editions use the same PC-derived soundtracks. Personal saves, emulator
configurations, ROMs, AmigaOS and developer SDK files are not included.
The manifests record file sizes and SHA-256 hashes.

Progress stays in memory during retries. Return to level selection with Escape
to save your scores and unlocks before switching off or forcibly quitting.
On the bootable ADF, Escape at level selection stays in the game: there is no
operating system to return to. WHDLoad retains its normal quit behavior.
Write-protection and save-error notices disappear after three seconds so level
selection remains visible. Dismissing the notice does not mean progress saved.

The ADF keeps using the physical drive it booted from (DF0 through DF3).
External-drive boot still requires firmware or a boot selector that supports it.
ENDING VISUAL AUDITION (development build)
Press F8 at the title menu to preview the secret ending with Focus played backwards
at its original sample rate (no PC pitch shift). Escape returns to the menu.
F7 tests the separate Hexagonest bonus section, with stretched Focus audio.
WHDLoad exit remains F10. Auditioning does not award completion or records.

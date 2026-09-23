# to generate assembler listing with LTO, add to LDFLAGS: -Wa,-adhln=$@.listing,--listing-rhs-width=200
# for better annotations add -dA -dP
# to generate assembler source with LTO, add to LDFLAGS: -save-temps=cwd

ifdef OS
	WINDOWS = 1
	SHELL = cmd.exe
endif

# Keep host-only tests/tools out of the freestanding Amiga link.
VPATH = support
cpp_sources :=
cpp_objects :=
c_sources := main.c system.c video.c coplist.c blitter.c trig.c input.c game.c patterns.c render.c hud.c player_shape.c pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_progress.c pc_menu.c pc_pulse.c pc_sfx.c pc_lifecycle.c pc_death.c pc_morph.c pc_projection.c pc_palette.c render_clip.c support/gcc8_c_support.c
# Default release behavior: omit the entire steering-assist translation unit.
CHEAT_MODE ?= 0
ifneq ($(CHEAT_MODE),0)
ifneq ($(CHEAT_MODE),1)
$(error CHEAT_MODE must be 0 or 1)
endif
c_sources += cheat.c
endif
# Soundtrack trials now only copy offline-predecoded PCM. Runtime codecs retired.
PCM_ASSET ?= out/courtesy
MUSIC_FIB_STREAM ?= 0
FIB_TRIAL_SONG ?= 1
FIB_TRIAL_PCM ?= 1
MUSIC_FIB_BENCH ?= 0
ifeq ($(MUSIC_FIB_BENCH),1)
$(error Runtime decompression trials are retired; use MUSIC_FIB_STREAM=1 for predecoded PCM)
endif
ifeq ($(MUSIC_FIB_STREAM),1)
ifneq ($(FIB_TRIAL_SONG)$(FIB_TRIAL_PCM),11)
$(error Runtime decompression is retired; soundtrack playback requires predecoded PCM)
endif
c_sources += fib_pcm.c pcm_lifecycle.c tests/fib_stream.c
SELFTEST_CFLAGS += -DMUSIC_FIB_STREAM=1 -DFIB_TRIAL_SONG=1 -DFIB_TRIAL_PCM=1
SELFTEST_CFLAGS += -DPCM_BANK_FIRST='"$(PCM_ASSET).boosted.pcm0"' -DPCM_BANK_SECOND='"$(PCM_ASSET).boosted.pcm1"'
VPATH += tests
endif
SOUND_EFFECTS ?= 1
SELFTEST_CFLAGS += -DSOUND_EFFECTS=$(SOUND_EFFECTS)
ifeq ($(SOUND_EFFECTS),1)
c_sources += sfx.c
endif
ifneq ($(filter 1,$(SOUND_EFFECTS) $(MUSIC_FIB_STREAM)),)
c_sources += paula_irq.c
endif
PC_CORE_SELFTEST ?= 0
ifeq ($(PC_CORE_SELFTEST),1)
c_sources += tests/core_checks.c tests/wave_checks.c tests/schedule_checks.c tests/progression_checks.c tests/menu_checks.c tests/lifecycle_checks.c tests/death_checks.c tests/clip_checks.c tests/projection_checks.c tests/trig_checks.c
SELFTEST_CFLAGS += -DPC_CORE_SELFTEST=1
VPATH += tests
endif
c_objects := $(addprefix obj/,$(notdir $(c_sources:.c=.o)))
s_sources := support/gcc8_a_support.s
s_objects := $(addprefix obj/,$(patsubst %.s,%.o,$(notdir $(s_sources))))
vasm_sources :=
vasm_objects := $(addprefix obj/, $(patsubst %.asm,%.o,$(notdir $(vasm_sources))))
objects := $(cpp_objects) $(c_objects) $(s_objects) $(vasm_objects)

# https://stackoverflow.com/questions/4036191/sources-from-subdirectories-in-makefile/4038459
# http://www.microhowto.info/howto/automatically_generate_makefile_dependencies.html

program = out/hexagon
OUT = $(program)
CC = m68k-amiga-elf-gcc
AS = m68k-amiga-elf-as
VASM = vasmm68k_mot
EXE2ADF = exe2adf
EXECRAM = execram

# zultra backend, self-checking the result (decompresses host-side, compares
# byte-for-byte) before writing it. Override on the command line to try
# another backend, e.g. make pack EXECRAM_FLAGS="--backend=auto"
# (store|inflate|zultra|zx0|salvador|shrinkler|auto - auto tries them all and
# keeps the smallest). https://github.com/going-digital/execram
EXECRAM_FLAGS ?= --backend=zultra

ifdef WINDOWS
	SDKDIR = $(abspath $(dir $(shell where $(CC)))..\m68k-amiga-elf\sys-include)
else
	SDKDIR = $(abspath $(dir $(shell which $(CC)))../m68k-amiga-elf/sys-include)
endif

# Pass build switches on the command line, e.g.
#   make EXTRA_CFLAGS="-DBUILD_DEBUG=0"        release build (no raster bar / debug hooks)
# PAL/NTSC timing is detected at startup; the same binary supports both.
EXTRA_CFLAGS ?=

CCFLAGS   = -g -MP -MMD -m68000 -Ofast -nostdlib -Wextra -Wno-unused-function -Wno-volatile-register-var -Wno-missing-field-initializers -fomit-frame-pointer -fno-tree-loop-distribution -flto -fwhole-program -fno-exceptions -ffunction-sections -fdata-sections $(EXTRA_CFLAGS) $(SELFTEST_CFLAGS) -include obj/cheat_config.h
CPPFLAGS  = $(CCFLAGS) -fno-rtti -fcoroutines -fno-use-cxa-atexit
ASFLAGS   = -mcpu=68000 -g --register-prefix-optional -I$(SDKDIR)
LDFLAGS   = -Wl,--emit-relocs,--gc-sections,-Ttext=0,-Map=$(OUT).map
VASMFLAGS = -m68000 -Felf -opt-fconst -nowarn=62 -dwarf=3 -quiet -x -I. -I$(SDKDIR)

# The canonical disk always boots the self-decrunching executable. Keep the
# raw executable/ELF for debugging; an unpacked disk is an explicit opt-in.
.DEFAULT_GOAL := all
.PHONY: all adf pack adf-unpacked
all: adf
adf: $(OUT).adf $(OUT)_packed.adf
pack: $(OUT)_packed.exe
adf-unpacked: $(OUT)_unpacked.adf

# exe2adf creates an AmigaDOS disk whose S/startup-sequence runs the payload.
# execram decrunches once at launch, before the game takes over the hardware.
$(OUT).adf: $(OUT)_packed.exe Makefile
	$(info Building packed ADF $@)
	@python3 -c "from pathlib import Path; import shutil; p = Path('$(OUT).adf-stage/hexagon'); p.parent.mkdir(parents=True, exist_ok=True); shutil.copyfile('$<', p)"
	@$(EXE2ADF) -i $(OUT).adf-stage/hexagon -l Hexagon -a $@

# Compatibility filename for existing launchers; identical to the default ADF.
$(OUT)_packed.adf: $(OUT).adf
	@python3 -c "import shutil; shutil.copyfile('$(OUT).adf', '$@')"

$(OUT)_unpacked.adf: $(OUT).exe
	$(info Building unpacked diagnostic ADF $@)
	@$(EXE2ADF) -i $< -l Hexagon -a $@

# Requires execram >= 1.3.0 to preserve Chip/ordinary memory classes.
# The packer verifies decompressed bytes against the input before writing.
$(OUT)_packed.exe: $(OUT).exe
	$(info execram-compressing $@)
	@$(EXECRAM) pack $(EXECRAM_FLAGS) $< $@

$(OUT).exe: $(OUT).elf
	$(info Elf2Hunk $(program).exe)
	@elf2hunk $(OUT).elf $(OUT).exe

$(OUT).elf: $(objects)
	$(info Linking $(program).elf)
	@$(CC) $(CCFLAGS) $(LDFLAGS) $(objects) -o $@
	@m68k-amiga-elf-objdump --disassemble --no-show-raw-ins --visualize-jumps -S $@ >$(OUT).s
ifeq ($(CHEAT_MODE),0)
	@python3 tools/check_no_cheats.py $@ $(OUT).map
endif

clean:
	$(info Cleaning...)
ifdef WINDOWS
	@del /q obj\* out\*
else
	@$(RM) obj/* out/*
endif

-include $(objects:.o=.d)

$(cpp_objects) : obj/%.o : %.cpp
	$(info Compiling $<)
	@$(CC) $(CPPFLAGS) -c -o $@ $(CURDIR)/$<

$(c_objects) : obj/%.o : %.c
	$(info Compiling $<)
	@$(CC) $(CCFLAGS) -c -o $@ $(CURDIR)/$<

$(s_objects): obj/%.o : %.s
	$(info Assembling $<)
	@$(AS) $(ASFLAGS) --MD $(@D)/$*.d -o $@ $(CURDIR)/$<

$(vasm_objects): obj/%.o : %.asm
	$(info Assembling $<)
	@$(VASM) $(VASMFLAGS) -dependall=make -depfile $(@D)/$*.d -o $@ $(CURDIR)/$<

.PHONY: test
HOST_CC ?= cc
test: test-palette test-progression test-menu test-lifecycle test-death test-sfx test-pulse test-video test-projection-edges test-player-shape test-pcm-lifecycle test-display-handoff
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_progress.c pc_menu.c pc_pulse.c pc_sfx.c pc_lifecycle.c pc_death.c pc_morph.c pc_projection.c render_clip.c tests/core_checks.c tests/wave_checks.c tests/schedule_checks.c tests/progression_checks.c tests/menu_checks.c tests/lifecycle_checks.c tests/death_checks.c tests/clip_checks.c tests/projection_checks.c tests/trig_checks.c tests/core_test.c -o out/core_test
	./out/core_test
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_morph.c pc_projection.c render_clip.c tests/wave_probe.c -o out/wave_probe
	python3 tests/compare_waves.py
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c tests/schedule_probe.c -o out/schedule_probe
	python3 tests/compare_schedule.py
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_morph.c tests/normal_run_test.c -o out/normal_run_test
	./out/normal_run_test

.PHONY: test-player-shape
test-player-shape: | out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror player_shape.c tests/player_shape_test.c -o out/player_shape_test
	./out/player_shape_test
	python3 tests/player_prerender_test.py

$(objects): | obj
$(OUT).elf: | out
obj out:
ifdef WINDOWS
	@if not exist "$@" mkdir "$@"
else
	@mkdir -p $@
endif

# A changed switch invalidates every object, including keyboard/input layout.
# The script preserves mtime when unchanged. No -B/clean is needed to turn off.
.PHONY: FORCE_CHEAT_CONFIG
obj/cheat_config.h: FORCE_CHEAT_CONFIG | obj
	@python3 tools/write_cheat_config.py $(CHEAT_MODE) $@
$(objects): obj/cheat_config.h

.PHONY: test-cheat
test-cheat:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror -DCHEAT_MODE=1 pc_core.c pc_world.c cheat.c tests/cheat_test.c -o out/cheat_test
	./out/cheat_test
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror -DCHEAT_MODE=1 pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_morph.c cheat.c tests/cheat_run_test.c -o out/cheat_run_test
	./out/cheat_run_test

.PHONY: check-no-cheats
check-no-cheats: $(OUT).elf
	@python3 tools/check_no_cheats.py $(OUT).elf $(OUT).map

# Deliberately override even an inherited/command-line CHEAT_MODE=1.
.PHONY: release
release:
	$(MAKE) CHEAT_MODE=0 EXTRA_CFLAGS="$(EXTRA_CFLAGS) -DBUILD_DEBUG=0" all check-no-cheats

.PHONY: test-palette
test-palette:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_palette.c tests/palette_probe.c -o out/palette_probe
	python3 tests/compare_palette.py

.PHONY: test-progression
test-progression:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_progress.c tests/progression_probe.c -o out/progression_probe
	python3 tests/compare_progression.py
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_core.c pc_world.c pc_waves.c pc_schedule.c pc_progress.c pc_morph.c tests/progression_run_test.c -o out/progression_run_test
	./out/progression_run_test

.PHONY: test-menu
test-menu:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_menu.c tests/menu_probe.c -o out/menu_probe
	python3 tests/compare_menu.py
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_menu.c tests/menu_checks.c tests/menu_test.c -o out/menu_test
	./out/menu_test

.PHONY: test-lifecycle
test-lifecycle:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_lifecycle.c tests/lifecycle_probe.c -o out/lifecycle_probe
	python3 tests/compare_lifecycle.py

.PHONY: test-death
test-death:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_death.c pc_lifecycle.c pc_morph.c pc_world.c pc_core.c tests/death_probe.c -o out/death_probe
	python3 tests/compare_death.py

ifeq ($(MUSIC_FIB_STREAM),1)
$(PCM_ASSET).boosted.pcm0: $(PCM_ASSET).pcm0 $(PCM_ASSET).pcm1 tools/audio/boost_pcm.py
	python3 tools/audio/boost_pcm.py $(PCM_ASSET) $(PCM_ASSET).boosted
$(PCM_ASSET).boosted.pcm1: $(PCM_ASSET).boosted.pcm0
	@test -f $@ || python3 tools/audio/boost_pcm.py $(PCM_ASSET) $(PCM_ASSET).boosted
obj/fib_stream.o: $(PCM_ASSET).boosted.pcm0 $(PCM_ASSET).boosted.pcm1
endif

.PHONY: test-pcm-lifecycle
test-pcm-lifecycle:
	mkdir -p out
	cc -Wall -Wextra -Werror pcm_lifecycle.c tests/pcm_lifecycle_test.c -o out/pcm_lifecycle_check
	out/pcm_lifecycle_check
	$(HOST_CC) -std=c99 -Wall -Wextra -Werror fib_pcm.c tests/pcm_seek_test.c -o out/pcm_seek_test
	out/pcm_seek_test

# Clips are converted offline; the target never decodes compressed audio.
out/sfx.pcm: tools/audio/prepare_sfx.py tools/audio/boost_pcm.py $(wildcard assets/sounds/*.ogg)
	python3 tools/audio/prepare_sfx.py
out/sfx_samples.h: out/sfx.pcm
	@test -f $@ || python3 tools/audio/prepare_sfx.py
obj/sfx.o: out/sfx.pcm out/sfx_samples.h

.PHONY: test-sfx
test-sfx:
	mkdir -p out
	$(HOST_CC) -Wall -Wextra -Werror pc_sfx.c tests/pc_sfx_test.c -o out/pc_sfx_test
	out/pc_sfx_test
	python3 tests/sfx_startup_test.py

obj/fib_stream.o: assets/music1.cues

.PHONY: test-pulse
test-pulse:
	@mkdir -p out
	$(HOST_CC) -std=c99 -Wall -Wextra -Werror pc_pulse.c tests/pc_pulse_test.c -o out/pc_pulse_test
	out/pc_pulse_test

.PHONY: test-video
.PHONY: test-display-handoff
test-display-handoff:
	python3 tests/display_handoff_test.py

test-video:
	@mkdir -p out
	$(HOST_CC) -std=c99 -Wall -Wextra -Werror video.c tests/video_test.c -o out/video_test
	out/video_test

.PHONY: test-projection-edges
test-projection-edges:
	@mkdir -p out
	$(HOST_CC) -std=c99 -O2 -Wall -Wextra -Werror pc_projection.c tests/projection_edges_test.c -o out/projection_edges_test
	./out/projection_edges_test

ifeq ($(MUSIC_FIB_STREAM),1)
obj/fib_stream.o: $(PCM_ASSET).pcm0 $(PCM_ASSET).pcm1
endif

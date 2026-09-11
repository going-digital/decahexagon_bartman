# to generate assembler listing with LTO, add to LDFLAGS: -Wa,-adhln=$@.listing,--listing-rhs-width=200
# for better annotations add -dA -dP
# to generate assembler source with LTO, add to LDFLAGS: -save-temps=cwd

ifdef OS
	WINDOWS = 1
	SHELL = cmd.exe
endif

subdirs := $(wildcard */)
VPATH = $(subdirs)
cpp_sources := $(wildcard *.cpp) $(wildcard $(addsuffix *.cpp,$(subdirs)))
cpp_objects := $(addprefix obj/,$(patsubst %.cpp,%.o,$(notdir $(cpp_sources))))
c_sources := $(wildcard *.c) $(wildcard $(addsuffix *.c,$(subdirs)))
c_objects := $(addprefix obj/,$(patsubst %.c,%.o,$(notdir $(c_sources))))
s_sources := support/gcc8_a_support.s support/depacker_doynax.s
s_objects := $(addprefix obj/,$(patsubst %.s,%.o,$(notdir $(s_sources))))
vasm_sources := $(wildcard *.asm) $(wildcard $(addsuffix *.asm, $(subdirs)))
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
SHRINKLER = Shrinkler

# Preset matches .vscode/amiga.json's "slow" shrinkler config (max compression,
# flashes DFF180 as it decrunches so you can see it's not hung on a real Amiga).
# Override on the command line for a quicker iterate, e.g.
#   make pack SHRINKLER_FLAGS="-h -o -1"        .vscode/amiga.json's "fast" preset
SHRINKLER_FLAGS ?= -h -f dff180 -9

ifdef WINDOWS
	SDKDIR = $(abspath $(dir $(shell where $(CC)))..\m68k-amiga-elf\sys-include)
else
	SDKDIR = $(abspath $(dir $(shell which $(CC)))../m68k-amiga-elf/sys-include)
endif

# Pass build switches on the command line, e.g.
#   make EXTRA_CFLAGS="-DBUILD_DEBUG=0"        release build (no raster bar / debug hooks)
#   make EXTRA_CFLAGS="-DTARGET_NTSC"          60Hz NTSC timing
EXTRA_CFLAGS ?=

CCFLAGS   = -g -MP -MMD -m68000 -Ofast -nostdlib -Wextra -Wno-unused-function -Wno-volatile-register-var -fomit-frame-pointer -fno-tree-loop-distribution -flto -fwhole-program -fno-exceptions -ffunction-sections -fdata-sections $(EXTRA_CFLAGS)
CPPFLAGS  = $(CCFLAGS) -fno-rtti -fcoroutines -fno-use-cxa-atexit
ASFLAGS   = -mcpu=68000 -g --register-prefix-optional -I$(SDKDIR)
LDFLAGS   = -Wl,--emit-relocs,--gc-sections,-Ttext=0,-Map=$(OUT).map
VASMFLAGS = -m68000 -Felf -opt-fconst -nowarn=62 -dwarf=3 -quiet -x -I. -I$(SDKDIR)

all: $(OUT).exe adf pack

# Bootable floppy image: exe2adf writes a disk with a bootblock that loads
# and runs the .exe directly, no AmigaDOS filesystem/Workbench needed - drop
# it straight into an emulator or write it to a real disk with a real Amiga.
adf: $(OUT).adf $(OUT)_packed.adf

$(OUT).adf: $(OUT).exe
	$(info Building ADF $(OUT).adf)
	@$(EXE2ADF) -i $(OUT).exe -l Decahexagon -a $(OUT).adf

$(OUT)_packed.adf: pack
	$(info Building ADF $(OUT)_packed.adf)
	@$(EXE2ADF) -i $(OUT)_packed.exe -l Decahexagon -a $(OUT)_packed.adf

# Shrinkler-compressed executable, built alongside the uncompressed one rather
# than replacing it - self-decrunching, same hunk format, just smaller and
# slower to load (decrunch time trades against SHRINKLER_FLAGS above).
pack: $(OUT)_packed.exe

$(OUT)_packed.exe: $(OUT).exe
	$(info Shrinkler-compressing $(program)_packed.exe)
	@$(SHRINKLER) $(SHRINKLER_FLAGS) $(OUT).exe $@

$(OUT).exe: $(OUT).elf
	$(info Elf2Hunk $(program).exe)
	@elf2hunk $(OUT).elf $(OUT).exe

$(OUT).elf: $(objects)
	$(info Linking $(program).elf)
	@$(CC) $(CCFLAGS) $(LDFLAGS) $(objects) -o $@
	@m68k-amiga-elf-objdump --disassemble --no-show-raw-ins --visualize-jumps -S $@ >$(OUT).s

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

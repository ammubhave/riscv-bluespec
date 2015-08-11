SHELL := /bin/sh
RISCV_PREFIX=riscv64-unknown-elf
CPP=$(RISCV_PREFIX)-gcc
CPPFLAGS=-static -fpic -fvisibility=hidden -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha

linux_src_dir := linux-3.14.29
build_dir := $(RISCV_BUILD)/programs
linux_build_dir := $(build_dir)/linux

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_GCC = $(RISCV_PREFIX)-gcc
RISCV_GCC_OPTS = -static -fpic -fvisibility=hidden -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha
RISCV_OBJDUMP = $(RISCV_PREFIX)-objdump --disassemble-all --disassemble-zeroes --section=.init.text --section=.text --section=.text.startup

#VPATH += $(asm_build_dir)

$(linux_src_dir)/vmlinux:
	$(MAKE) -C $(linux_src_dir) ARCH=riscv -j

$(linux_build_dir)/vmlinux.vmh: $(linux_src_dir)/vmlinux
	@echo "@0" > $@
	elf2hex 8 16384 $(RISCV_HOME)/programs/riscv-pk/build/bbl >> $@
	(od -An -v -tx8 -w8 $< | sed 's/ //g') >> $@

$(linux_build_dir)/vmlinux.out_PHONY: $(linux_build_dir)/vmlinux.vmh
	@ln -sf $< $(PROC_BUILD_DIR)/memory.vmh
	@tput setaf 1 && $(MAKE) -C $(PROC_BUILD_DIR) --no-print-directory run > $(@:.out_PHONY=.out)
	@tput sgr0; tput bold

run: $(linux_build_dir)/vmlinux.out_PHONY

junk += $(linux_build_dir)/vmlinux.out_PHONY $(linux_build_dir)/vmlinux.vmh

#------------------------------------------------------------
# Default

$(linux_build_dir):
	mkdir -p $(linux_build_dir)

all: $(linux_build_dir) $(linux_build_dir)/vmlinux.vmh

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(junk)

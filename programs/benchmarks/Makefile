#=======================================================================
# UCB VLSI FLOW: Makefile for riscv-bmarks
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#
SHELL := /bin/bash

default: all

bmarkdir = .
build_dir := $(RISCV_BUILD)/programs
bmarks_build_dir := $(build_dir)/benchmarks

#instname = riscv-bmarks
#instbasedir = $(UCB_VLSI_HOME)/install

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

bmarks = \
	median \
	qsort \
	towers \
	vvadd \
	multiply \
	#rsort \
	#mm \
	#dhrystone \
	#spmv \
	#mt-vvadd \
	#mt-matmul \
	#vec-fft \
	#vec-vvadd \
	#vec-cmplxmult \
	#vec-matmul \

bmarks_host = \
	median \
	qsort \
	towers \
	vvadd \
	multiply \
	spmv \
	vec-vvadd \
	vec-cmplxmult \
	vec-matmul \

#--------------------------------------------------------------------
# Build rules -march=RVIMAFDXhwacha
#--------------------------------------------------------------------

HOST_OPTS = -std=gnu99 -DPREALLOCATE=0 -DHOST_DEBUG=1
HOST_COMP = gcc $(HOST_OPTS)

RISCV_PREFIX=riscv64-unknown-elf
RISCV_GCC = $(RISCV_PREFIX)-gcc
RISCV_GCC_OPTS = -static -Wa,-march=RVIMF -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf
RISCV_LINK = $(RISCV_GCC) -T $(RISCV_HOME)/programs/benchmarks/common/test.ld $(incs) -I$(bmarks_build_dir)/
RISCV_LINK_MT = $(RISCV_GCC) -T $(RISCV_HOME)/programs/benchmarks/common/test-mt.ld
RISCV_LINK_OPTS = -nostdlib -nostartfiles -ffast-math -lc -lgcc
RISCV_OBJDUMP = $(RISCV_PREFIX)-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.data

VPATH += $(addprefix $(bmarkdir)/, $(bmarks))
VPATH += $(bmarkdir)/common
VPATH += $(bmarks_build_dir)

incs  += -I$(bmarkdir)/../env -I$(bmarkdir)/common $(addprefix -I$(bmarkdir)/, $(bmarks))
objs  :=

include $(patsubst %, $(bmarkdir)/%/bmark.mk, $(bmarks))

#------------------------------------------------------------
# Build and run benchmarks on riscv simulator

bmarks_riscv_bin  = $(addsuffix .riscv,  $(bmarks))
bmarks_riscv_dump = $(addsuffix .riscv.dump, $(bmarks))
bmarks_riscv_vmh = $(addsuffix .riscv.vmh, $(bmarks))
bmarks_riscv_out  = $(addsuffix .riscv.out_PHONY,  $(bmarks))

bmarks_defs   = -DPREALLOCATE=1 -DHOST_DEBUG=0
bmarks_cycles = 80000

$(bmarks_riscv_vmh): %.riscv.vmh: %.riscv
	@echo $(basename $<)
	@echo "@0" > $(bmarks_build_dir)/$@
	elf2hex 8 32768 $(bmarks_build_dir)/$< >> $(bmarks_build_dir)/$@

$(bmarks_riscv_dump): %.riscv.dump: %.riscv
	$(RISCV_OBJDUMP) $(bmarks_build_dir)/$< > $(bmarks_build_dir)/$@

$(bmarks_riscv_out): %.riscv.out_PHONY: %.riscv.vmh
	@echo $<
	@ln -sf $< $(PROC_BUILD_DIR)/memory.vmh
	@tput setaf 1 && $(MAKE) -C $(PROC_BUILD_DIR) --no-print-directory run > $(bmarks_build_dir)/$(@:.out_PHONY=.out)
	@tput sgr0; tput bold

#	$(RISCV_SIM) $< > $(bmarks_build_dir)/$@
# cd $(build_dir)/$(RISCV_SIMULATOR) && $(RISCV_HOME)/utility-bins/redirect $(build_dir)/$(RISCV_SIMULATOR)/sim $(build_dir)/$(RISCV_SIMULATOR)/benchmarks/$@

%.o: %.c
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $(bmarks_defs) \
	             -c $(incs) $< -o $(bmarks_build_dir)/$@

%.o: %.S
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $(bmarks_defs) -D__ASSEMBLY__=1 \
	             -c $(incs) $< -o $(bmarks_build_dir)/$@

riscv: $(build_dir) $(bmarks_riscv_dump) $(bmarks_riscv_vmh)
run-riscv: $(bmarks_riscv_out)
run: run-riscv

#echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
#	       $(bmarks_riscv_out); echo;

junk += $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(bmarks_riscv_vmh) $(bmarks_riscv_out)

#------------------------------------------------------------
# Build and run benchmarks on host machine

bmarks_host_bin = $(addsuffix .host, $(bmarks_host))
bmarks_host_out = $(addsuffix .host.out, $(bmarks_host))

$(bmarks_host_out): %.host.out: %.host
	./$< > $@

host: $(bmarks_host_bin)
run-host: $(bmarks_host_out)
	echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(bmarks_host_out); echo;

junk += $(bmarks_host_bin) $(bmarks_host_out)

#------------------------------------------------------------
# Default
$(build_dir):
	mkdir -p $(build_dir)
	mkdir -p $(bmarks_build_dir)

all: $(build_dir) riscv

#------------------------------------------------------------
# Install

#date_suffix = $(shell date +%Y-%m-%d_%H-%M)
#install_dir = $(instbasedir)/$(instname)-$(date_suffix)
#latest_install = $(shell ls -1 -d $(instbasedir)/$(instname)* | tail -n 1)

#install:
#	mkdir $(install_dir)
#	cp -r $(bmarks_riscv_bin) $(bmarks_riscv_dump) $(install_dir)

#install-link:
#	rm -rf $(instbasedir)/$(instname)
#	ln -s $(latest_install) $(instbasedir)/$(instname)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(objs) $(addprefix $(bmarks_build_dir)/, $(junk))

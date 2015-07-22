SHELL := /bin/sh
RISCV_PREFIX=riscv64-unknown-elf
CPP=$(RISCV_PREFIX)-gcc
CPPFLAGS=-static -fpic -fvisibility=hidden -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha

#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

build_dir := $(RISCV_BUILD)/programs
asm_build_dir := $(build_dir)/assembly

include rv64ui/Makefrag

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_GCC = $(RISCV_PREFIX)-gcc
RISCV_GCC_OPTS = -static -fpic -fvisibility=hidden -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha
RISCV_OBJDUMP = $(RISCV_PREFIX)-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.data

VPATH += $(asm_build_dir)

#------------------------------------------------------------
# Build assembly tests

%.vmh: %
	@echo "@0" > $(asm_build_dir)/$@
	elf2hex 8 8192 $(asm_build_dir)/$< >> $(asm_build_dir)/$@

#elf2hex 64 16384 $(asm_build_dir)/$< >> $(asm_build_dir)/$@
#(elf2hex 8 8192 $(asm_build_dir)/$< 2> /dev/null || elf2hex 512 16384 $<) >> $(asm_build_dir)/$@
#(elf2hex 8 8192 $(asm_build_dir)/$< 2> /dev/null || elf2hex 64 16384 $<) >> $(asm_build_dir)/$@

%.dump: %
	$(RISCV_OBJDUMP) $(asm_build_dir)/$< > $(asm_build_dir)/$@

%.out_PHONY: $(addprefix $(asm_build_dir)/, %.vmh)
	@echo $<
	@ln -sf $< $(PROC_BUILD_DIR)/memory.vmh
	@tput setaf 1 && $(MAKE) -C $(PROC_BUILD_DIR) --no-print-directory run > $(asm_build_dir)/$@
	@tput sgr0; tput bold

#	@mkdir -p $(build_dir)/$(RISCV_SIMULATOR)/assembly
#	@ln -sf $< $(build_dir)/$(RISCV_SIMULATOR)/memory.vmh
#	-@pkill -9 bluetcl > /dev/null 2> /dev/null || true
#	@echo $(basename $<)
#	@echo -e -n "\t"
#	@tput setaf 1 && cd $(build_dir)/$(RISCV_SIMULATOR) && $(RISCV_HOME)/utility-bins/redirect $(build_dir)/$(RISCV_SIMULATOR)/sim $(build_dir)/$(RISCV_SIMULATOR)/assembly/$@ &
#	@tput sgr0; tput bold
#	@sleep 1; cd $(build_dir)/$(RISCV_SIMULATOR)/bsim-scemi && $(RISCV_HOME)/utility-bins/redirect $(build_dir)/$(RISCV_SIMULATOR)/bsim-scemi/run $(build_dir)/$(RISCV_SIMULATOR)/assembly/runOut

define compile_template

$$($(1)_p_tests): $(1)-p-%: $(1)/%.S
	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I../env/p -Imacros/scalar -T../env/p/link.ld $$< -o $(asm_build_dir)/$$@
$(1)_tests += $$($(1)_p_tests)

#$$($(1)_pt_tests): $(1)-pt-%: $(1)/%.S
#	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(asm_src_dir)/../env/pt -I$(asm_src_dir)/macros/scalar -T$(asm_src_dir)/../env/pt/link.ld $$< -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_pt_tests)

#$$($(1)_pm_tests): $(1)-pm-%: $(1)/%.S
#	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(asm_src_dir)/../env/pm -I$(asm_src_dir)/macros/scalar -T$(asm_src_dir)/../env/pm/link.ld $$< -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_pm_tests)

#$$($(1)_v_tests): $(1)-v-%: $(1)/%.S
	#$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -std=gnu99 -O2 -I$(asm_src_dir)/../env/v -I$(asm_src_dir)/macros/scalar -T$(asm_src_dir)/../env/v/link.ld $(asm_src_dir)/../env/v/entry.S $(asm_src_dir)/../env/v/vm.c $$< -lc -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_v_tests)

#$$($(1)_p_vec_tests): $(1)-p-vec-%: $(1)/%.S
	#$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(asm_src_dir)/../env/p -I$(asm_src_dir)/macros/vector -T$(asm_src_dir)/../env/p/link.ld $$< -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_p_vec_tests)
#
#$$($(1)_pt_vec_tests): $(1)-pt-vec-%: $(1)/%.S
#	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(asm_src_dir)/../env/pt -I$(asm_src_dir)/macros/vector -T$(asm_src_dir)/../env/pt/link.ld $$< -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_pt_vec_tests)

#$$($(1)_v_vec_tests): $(1)-v-vec-%: $(1)/%.S
#	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -std=gnu99 -O2 -I$(asm_src_dir)/../env/v -I$(asm_src_dir)/macros/vector -T$(asm_src_dir)/../env/v/link.ld $(asm_src_dir)/../env/v/entry.S $(asm_src_dir)/../env/v/vm.c $$< -lc -o $(asm_build_dir)/$$@
#$(1)_tests += $$($(1)_v_vec_tests)

$(1)_tests_dump = $$(addsuffix .dump, $$($(1)_tests))
$(1)_tests_vmh = $$(addsuffix .vmh, $$($(1)_tests))

$(1): $$($(1)_tests_dump) $$($(1)_tests_vmh)

.PHONY: $(1)

tests += $$($(1)_tests)

endef

$(eval $(call compile_template,rv64ui))

tests_dump = $(addsuffix .dump, $(tests))
tests_vmh = $(addsuffix .vmh, $(tests))
tests_out = $(addsuffix .out_PHONY, $(tests))

run: $(tests_out)

junk += $(tests) $(tests_dump) $(tests_vmh) $(tests_out)

#------------------------------------------------------------
# Default

$(asm_build_dir):
	mkdir -p $(asm_build_dir)

all: $(asm_build_dir) $(tests_dump) $(tests_vmh)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(addprefix $(asm_build_dir)/, $(junk))

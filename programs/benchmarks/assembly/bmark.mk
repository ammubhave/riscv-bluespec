#=======================================================================
# UCB CS250 Makefile fragment for benchmarks
#-----------------------------------------------------------------------
#
# Each benchmark directory should have its own fragment which
# essentially lists what the source files are and how to link them
# into an riscv and/or host executable. All variables should include
# the benchmark name as a prefix so that they are unique.
#

assembly_c_src = \
	syscalls.c \

assembly_riscv_src = \
    add.S \
	crt.S \

assembly_c_objs     = $(patsubst %.c, %.o, $(assembly_c_src))
assembly_riscv_objs = $(patsubst %.S, %.o, $(assembly_riscv_src))

assembly_riscv_bin = assembly.riscv
$(assembly_riscv_bin): $(assembly_c_objs) $(assembly_riscv_objs)
	cd $(bmarks_build_dir); $(RISCV_LINK) $(assembly_c_objs) $(assembly_riscv_objs) -o $(assembly_riscv_bin) $(RISCV_LINK_OPTS)

junk += $(assembly_c_objs) $(assembly_riscv_objs) \
        $(assembly_host_bin) $(assembly_riscv_bin)

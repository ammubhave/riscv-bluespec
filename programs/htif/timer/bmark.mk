#=======================================================================
# UCB CS250 Makefile fragment for benchmarks
#-----------------------------------------------------------------------
#
# Each benchmark directory should have its own fragment which
# essentially lists what the source files are and how to link them
# into an riscv and/or host executable. All variables should include
# the benchmark name as a prefix so that they are unique.
#

timer_c_src = \
	timer_main.c \
	timer.c \
	syscalls.c \

timer_riscv_src = \
	crt.S \

timer_c_objs     = $(patsubst %.c, %.o, $(timer_c_src))
timer_riscv_objs = $(patsubst %.S, %.o, $(timer_riscv_src))

timer_host_bin = timer.host
$(timer_host_bin): $(timer_c_src)
	$(HOST_COMP) $^ -o $(timer_host_bin)

timer_riscv_bin = timer.riscv
$(timer_riscv_bin): $(timer_c_objs) $(timer_riscv_objs)
	cd $(bmarks_build_dir); $(RISCV_LINK) $(timer_c_objs) $(timer_riscv_objs) -o $(timer_riscv_bin) $(RISCV_LINK_OPTS)

junk += $(timer_c_objs) $(timer_riscv_objs) \
        $(timer_host_bin) $(timer_riscv_bin)

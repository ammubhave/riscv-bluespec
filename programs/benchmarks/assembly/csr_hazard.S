# See LICENSE for license details.

#*****************************************************************************
# simple.S
#-----------------------------------------------------------------------------
#
# This is the most basic self checking test. If your simulator does not
# pass thiss then there is little chance that it will pass any of the
# more complicated self checking tests.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV64U
RVTEST_CODE_BEGIN

  li x28, 0;
  li t3, 2;
  li a0, 4;
  csrw stats, a0;
  csrr a1, stats;
  beq a0, a1, pass;
	
  RVTEST_FAIL

pass:
  RVTEST_PASS

end: j end;

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END

#include "debug.h"
program debug_test
  use debug
  implicit none

  assert(1==1)
  assert(1==2)

end program debug_test

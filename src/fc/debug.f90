!> author: FANGCHAO
!  data: 2017/7/12
!
!## description
! this module print the debug information of code.
!
!@warning
! This module must used together with `#include "debug.h"`,which define the user
! interface, mainly contains:
!```C
!#define info(unit,format) if(unit<=debug_level_) write(debug_unit(unit), format)
!#define error(X) call debug_error(X,__FILE__,__LINE__)
!
!```
!
!@note
! the default `debug_level_=1`, one can use `call set_debug_level(level=)` to set
! the `debug_level_`.`,only `level<debug_level_`,`info(level,fmt)` will print
! information. when 'level<1',print to `ERROR_UNIT`,else print to `OUTPUT_UNIT`.
! use `error(msg)` to print error message.

module debug
  use iso_fortran_env, only:eunit_ => error_unit,ounit_ => output_unit
  use face, only: colorize
  implicit none

  public :: set_debug_level, debug_unit, debug_error, debug_level_

  private

  integer, save :: debug_level_  = 1


contains
  !=============================================================================
  subroutine set_debug_level(level)
    !< set debug_level_, only print `level<debug_level_` information
    integer,intent(in) :: level

    debug_level_ = level

  end subroutine set_debug_level
  !=============================================================================
  function debug_unit(unit)
    !< choose where to print, level<1',print to `ERROR_UNIT`, else print to `OUTPUT_UNIT`
    integer, intent(in) :: unit
    integer :: debug_unit

    if(unit<1) then
       debug_unit = eunit_
    else
       debug_unit = ounit_
    end if

  end function debug_unit
  !=============================================================================
  subroutine debug_error(msg,file,line)
    !< print error message, contains which file and line
    character(*), intent(in) :: msg,file
    integer, intent(in) :: line
    character(8) :: ls

    write(ls,'(i0)') line

    write(eunit_,*)              colorize('# Error',[5,0,0])
    write(eunit_,'(1x,3a,i0,a)') 'Source loction: (' //  colorize(file,[5,0,0])//', '// colorize(trim(ls),[5,0,0])//' )'
    write(eunit_,*)              'Error message : '//colorize(msg,[5,0,0])

    stop
  end subroutine debug_error

end module debug

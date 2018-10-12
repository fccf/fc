module sys

  implicit none

  public :: mkdir, cwd, cd

  private

contains
  !=============================================================================
  subroutine mkdir(dir)
    !< make directory
    character(*), intent(in):: dir

    call system('mkdir '//dir)

  end subroutine mkdir
  !=============================================================================
  function cwd() result(path)
    !< get current work directory
    character(:),allocatable :: path
    character(256) :: path_

    integer :: ierr

    ierr = getcwd(path_)
    path = trim(adjustl(path_))

  end function cwd
  !=============================================================================
  subroutine cd(dir)
    !< change directory
    character(*),intent(in) :: dir
    integer :: ierr

    ierr = chdir(dir)

  end subroutine cd

end module sys

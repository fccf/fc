module file

  implicit none

  public :: file_ext,  &
          & file_base, &
          & file_name, &
          & file_path

  private

#ifdef LINUX
  character, parameter :: path_separator_ = '/'
#else
  character, parameter :: path_separator_ = '\'
#endif

contains
  !=============================================================================
  function file_ext(file) result(rst)
    !< file's extension name
    character(*), intent(in)      :: file
    character(len=:), allocatable :: rst

    integer :: st

    st  = index(file, '.', back = .true.)
    rst = file(st+1:)

  end function file_ext
  !=============================================================================
  function file_base(file) result(rst)
    !< file's base name
    character(*),intent(in)      :: file
    character(len=:),allocatable :: rst

    integer :: st,lst

    st  = index(file, path_separator_, back = .true.)
    lst = index(file, '.', back = .true.)

    rst = file(st+1:lst-1)

  end function file_base
  !=============================================================================
  function file_name(file) result(rst)
    !< file's name, remove path
    character(*),intent(in)      :: file
    character(len=:),allocatable :: rst

    integer :: st

    st  = index(file, path_separator_, back = .true.)

    rst = file(st+1:)

  end function file_name
  !=============================================================================
  function file_path(file) result(rst)
    !< file's path
    character(*), intent(in) :: file
    character(len=:),allocatable :: rst

    integer :: lst

    lst  = index(file, path_separator_, back = .true.)

    rst = file(1:lst-1)

  end function file_path

end module file

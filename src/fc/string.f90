module string

  implicit none

  public :: upper, &
            lower, &
            insert, &
            replace, &
            replace_all, &
            delete, &
            count_words, &
            join, &
            split, &
            start_with, &
            end_with, &
            str, &
            to_str, &
            is_logical, &
            is_int, &
            is_real, &
            to_real, &
            to_int, &
            to_logical


  private

  interface str
     module procedure :: str_r8, str_r8Format, str_i4, str_bool
  end interface str


  interface to_str
    module procedure :: scalar_to_string, vector_to_string, matrix_to_string
  end interface to_str


contains
  !=============================================================================
  elemental function upper(s) result(rst)
    !< Returns string in uppercase
    character(*), intent(in)  :: s
    character(len(s))         :: rst

    integer :: i, dis

    rst = s
    dis = ichar('A')-ichar('a')
    do i = 1, len(rst)
       if ( ichar(rst(i:i)) >= ichar('a') .and. &
            ichar(rst(i:i)) <= ichar('z')) then

          rst(i:i) = char(ichar(rst(i:i)) + dis)
       end if
    end do
  end function upper
  !=============================================================================
  elemental function lower(s) result(rst)
    !< Returns string in lowercase
    character(*), intent(in) :: s
    character(len(s))        :: rst

    integer :: i, dis

    rst = s
    dis = ichar('A')-ichar('a')
    do i = 1, len(rst)
       if ( ichar(rst(i:i)) >= ichar('A') .and.&
            ichar(rst(i:i)) <= ichar('Z')) then
          rst(i:i) = char(ichar(rst(i:i)) - dis)
       end if
    end do
  end function lower
  !=============================================================================
  elemental function insert(str,sub,pos) result(new)
    !< insert string at pos (position)
    character(len=*), intent(in)  :: str
    character(len=*), intent(in)  :: sub
    integer, intent(in)           :: pos
    character(len(str)+len(sub))  :: new

    integer :: ipos

    ipos = min(max(1,pos),len(str))
    new  = str(1:ipos)//sub//str(ipos+1:)
  end function insert

  !=============================================================================
  elemental function replace(str, sub,rep) result(new)
    !< Return a string with first occurrences of substring sub replaced by rep.
    character(len=*), intent(in)          :: str
    character(len=*), intent(in)          :: sub
    character(len=*), intent(in)          :: rep
    character(len(str)-len(sub)+len(rep)) :: new

    integer :: pos

    pos = index(string=str, substring=sub)

    if (pos>0) then

       if (pos==1) then

          new = rep//str(len(sub)+1:)
       else
          new = str(1:pos-1)//rep//str(pos+len(sub):)
       endif

    endif

  end function replace
  !=============================================================================
  function replace_all(str, sub, rep) result(new)
    !< Return a string with all occurrences of substring sub replaced by rep.
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: sub
    character(len=*), intent(in) :: rep
    character(len=:),allocatable :: new

    new = str

    do
       if(index(new,sub) > 0) then
          new = replace(new, sub, rep)
       else
          exit
       endif

    enddo

  end function replace_all
  !=============================================================================
  elemental function delete(str, sub) result(new)
    !< Return a string with first  substring sub deleted
    character(len=*), intent(in)  :: str
    character(len=*), intent(in)  :: sub
    character(len(str))           :: new

    integer :: pos

    pos = index(string=str, substring=sub)

    if (pos>0) then

       if (pos==1) then

          new = str(len(sub)+1:)
       else
          new = str(1:pos-1)//str(pos+len(sub):)
       endif

    endif

    new = trim(adjustl(new))
  end function delete
  !=============================================================================
  pure function count_words(str,sub) result(rst)
    !< count the word `sub` in `str`
    character(*), intent(in):: str
    character(*), intent(in):: sub

    integer :: rst
    integer :: n1,n2

    rst = 0
    if (len(sub)>len(str)) return

    n1 = 1
    do

       n2 = index(string=str(n1:), substring=sub)
       if (n2 > 0) then
          rst = rst + 1
          n1 = n1 + n2 + len(sub)
       else
          exit
       endif

    enddo

    return

  endfunction count_words
  !=============================================================================
  subroutine split(str, words, nwords, sep)
    !< Return a list of words in the string
    character(len=*), intent(in)             :: str
    character(len=*),allocatable,intent(out) :: words(:)
    integer, intent(out), optional           :: nwords
    character(len=*), intent(in), optional   :: sep

    character(len =:),allocatable :: space, table
    character(len =:),allocatable :: str_tmp, sep_

    integer :: i,c,n

    space = ' '
    table = char(9)
    sep_  = space

    if(present(sep)) sep_ = sep

    str_tmp = replace_all(trim(adjustl(str)), table ,sep_)
    str_tmp = str_tmp//sep_

    n = 0
    do
       c = index(str_tmp,sep_)
       if(c == 1) exit
       str_tmp = trim(adjustl(str_tmp(c+1:)))//sep_
       n = n+1
    enddo

    allocate(words(n))

    str_tmp = replace_all(trim(adjustl(str)), table ,sep_)
    str_tmp = str_tmp//sep_
    do i = 1,n

       c = index(str_tmp,sep_)
       words(i) = str_tmp(1:c-1)
       str_tmp = trim(adjustl(str_tmp(c+1:)))//sep_

    end do

    if(allocated(str_tmp))  deallocate(str_tmp)

    if(present(nwords)) nwords = n

  end subroutine split
  !=============================================================================
  function join(words, sep) result(str)
    !< Return the string that is the join of an array of characters.
    character(len=*), intent(in)           :: words(1:)
    character(len=*), intent(in), optional :: sep
    character(len=:), allocatable          :: str
    character(len=:), allocatable          :: sep_

    integer :: i

    sep_ = ' '
    if (present(sep)) sep_ = sep

    str = ''

    do i=2, size(words,1)
       if (trim(words(i))/='') str = str//sep_//trim(adjustl(words(i)))
    enddo

    if (words(1)/='') then
       str = trim(adjustl(words(1)))//str
    else
       str = str(len(sep_)+1:len(str))

    endif

    return

  end function join
  !=============================================================================
  pure function start_with(str, prefix) result(rst)
    !< Return true if a string starts with a specified prefix.
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: prefix

    logical :: rst

    rst= .false.

    if (len(prefix)<=len(str)) then
       rst = index(str, prefix)==1
    endif

  end function start_with
  !=============================================================================
  pure function end_with(str, suffix) result(rst)
    !< Return true if a string end with a specified suffix.
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: suffix

    logical :: rst

    rst= .false.

    if (len(suffix)<=len(str)) then
       rst = index(str, suffix, back = .true.)==(len(str) - len(suffix) + 1)
    endif

  end function end_with
  !=============================================================================
  pure function str_r8(real_in,digits_in) result(str)
    !< convert real to string
    real, intent(in)            :: real_in
    integer,optional,intent(in) :: digits_in

    character(len = 15)         :: str
    character(9)                :: fmt

    integer :: digits,width
    real(8) :: rtemp

    digits = 6
    if(present(digits_in)) digits = digits_in
    width = 15

    str = ''
    rtemp = abs(real_in)

    if (rtemp >= 0.0e-1_8 .and. rtemp < 1.0_8) then
       write(fmt, '("(ES",I2,".",I2,")")') width, digits-1
    elseif (rtemp >= 1.0_8 .and. rtemp < 10.0_8) then
       write(fmt, '("(F",I2,".",I2,")")') width, max(digits-1, 0)
    elseif (rtemp >= 10.0_8 .and. rtemp < 100.0_8) then
       write(fmt, '("(F",I2,".",I2,")")') width, max(digits-2, 0)
    elseif (rtemp >= 100.0_8 .and. rtemp < 1000.0_8) then
       write(fmt, '("(F",I2,".",I2,")")') width, max(digits-3, 0)
    elseif (rtemp >= 100.0_8 .and. rtemp < 10000.0_8) then
       write(fmt, '("(F",I2,".",I2,")")') width, max(digits-4, 0)
    elseif (rtemp >= 10000.0_8 .and. rtemp < 100000.0_8) then
       write(fmt, '("(F",I2,".",I2,")")') width, max(digits-5, 0)
    else
       write(fmt, '("(ES",I2,".",I2,")")') width, digits - 1
    end if

    write(str,fmt) real_in
    str = trim(adjustl(str))

  end function str_r8
  !=============================================================================
  pure function str_r8Format(r8,format) result(str)
    !< covert real to string with format
    real, intent(in)             :: r8
    character(*), intent(in)     :: format
    character(len=:),allocatable :: str

    character(len=:),allocatable :: lformat
    character(len=20) :: str_temp

    lformat = '('//format//')'

    write(str_temp,lformat) r8
    str = trim(adjustl(str_temp))

    return

  end function str_r8Format
  !=============================================================================
  pure function str_i4(i4) result(str)
    !< convert integer to string
    integer, intent(in)          :: i4
    character(len=:),allocatable :: str

    character(len=20) :: str_temp

    write(str_temp,*) i4
    str = trim(adjustl(str_temp))

  end function str_i4
  !=============================================================================
  pure function str_bool(log) result(str)
    !< convert logical to string
    logical, intent(in)          :: log
    character(len=:),allocatable :: str

    if(log .eqv. .true.) then
       str = 'TRUE'
    else
       str = 'FALSE'
    end if

    return

  end function str_bool
  !=============================================================================
  elemental function is_logical(str)
    character(*),intent(in):: str
    logical :: is_logical
    is_logical = .false.
    select case (lower(trim(adjustl(str))))
    case ('yes', 'on', 'true', 'no', 'off', 'false')
      is_logical = .true.
    end select
  endfunction is_logical
  !=============================================================================
  elemental function is_int(str)
    character(*),intent(in)   :: str
    logical                   :: is_int
    integer                   :: ios
    integer                   :: int
    is_int = .false.
    read(str, * ,iostat= ios) int
    if(ios== 0) is_int = .true.
  endfunction is_int
  !=============================================================================
  elemental function is_real(str)
    character(*), intent(in) :: str
    logical                  :: is_real
    real                     :: r
    integer                  :: ios     !< The iosof I/O
    is_real = .false.
    read(str, * ,iostat= ios) r
    if(ios== 0) is_real = .true.
  endfunction is_real
  !=============================================================================
  elemental function to_real(str)
    character(*),intent(in) :: str
    real    :: to_real
    integer :: ios

    read(str, * , iostat= ios) to_real
    ! if(ios /= 0) error stop 'string "'//str//'" is not a real number'
  endfunction to_real
  !=============================================================================
  elemental function to_int(str)
    character(*),intent(in) :: str
    integer :: to_int
    integer :: ios

    read(str, * ,iostat= ios) to_int
    ! if(ios /= 0) error stop 'string "'//str//'" is not a integer number'
  endfunction to_int
  !=============================================================================
  elemental function to_logical(str)
    character(*),intent(in) :: str
    logical :: to_logical

    to_logical = .false.
    select case (lower(trim(adjustl(str))))
    case ('t','yes', 'on', 'true')
      to_logical = .true.
    case ('f','no', 'off', 'false')
      to_logical = .false.
    case default
      to_logical = .false.
      ! error stop 'string "'//trim(adjustl(str))//'" is not a logical identification!'
    end select

  endfunction to_logical
  !=============================================================================
  pure function scalar_to_string(value) result(s)
    !< convert any scalar type (integer, real, logical, character(*)) to string
    class(*), intent(in)      :: value
    character(:), allocatable :: s
    integer, parameter        :: max_num_len_ = 32
    character(max_num_len_)   :: ls

    select type(v_p => value)
    type is(integer)
      write(ls,'(i0)') v_p
      s = trim(adjustl(ls))
    type is(real)
      write(ls,fmt=*) v_p
      s = trim(adjustl(ls))
    type is(logical)
      if (v_p) then
        write(ls,'(a)') 'true'
      else
        write(ls,'(a)') 'false'
      end if
      s = trim(adjustl(ls))
    type is(character(*))
      s = trim(adjustl(v_p))
    class default
      write(ls,'(a)') '***'
      s = trim(adjustl(ls))
    end select
  end function scalar_to_string
  !=============================================================================
  pure function vector_to_string(value, vsep, shell) result(s)
    !< convert any vector type (integer, real, logical, character(*)) to string
    class(*), intent(in) :: value(:)
    character(*), intent(in), optional :: vsep   !< vector separator ',', ' '
    logical,intent(in), optional :: shell        !< if have the shell []
    character(:), allocatable :: s

    character(:),allocatable :: lsep
    logical :: lshell
    integer :: n

    lsep = ','      !< default vector separator
    lshell = .TRUE. !< default shell = true
    if(present(vsep)) lsep = vsep !< local optional argument
    if(present(shell)) lshell = shell !< another local argument

    s = ''
    if(lshell) s = '['
    do n = 1, size(value)
      if (n > 1) s = s//lsep
      s = s // to_str(value(n))    !< str => scalar_to_string
    end do
    if(lshell) s = s// ']'

  end function vector_to_string
  !=============================================================================
  pure function matrix_to_string(value, vsep, msep, shell) result(s)
    class(*), intent(in) :: value(:,:)
    character(*), intent(in), optional :: vsep  !< vector separator ',', ' '
    character(*), intent(in), optional :: msep  !< matrix separator ';', new_line('a')
    logical,intent(in), optional :: shell       !< if have the shell []

    character(:), allocatable :: s

    character(:),allocatable :: lvsep, lmsep
    logical :: lshell
    integer   :: n, m
    lvsep = ','
    lmsep = ';'
    lshell = .TRUE.
    if(present(vsep)) lvsep = vsep
    if(present(msep)) lmsep = msep
    if(present(shell)) lshell = shell

    s = ''
    if(lshell) s = '['
    do n = 1, size(value,2)
      if (n > 1) s = s//lmsep
      if(lshell) s = s//'['
      do m = 1, size(value, 1)
        if (m > 1) s = s//lvsep
        s = s // to_str(value(m,n))    !< str => scalar_to_string
      enddo
      if(lshell) s = s//']'
    enddo
    if(lshell) s = s//']'

  end function matrix_to_string

end module string

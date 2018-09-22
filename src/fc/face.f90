module face
  use iso_fortran_env, only: ounit_ => output_unit
  use string, only: to_str
  implicit none

  private

  public :: colorize,    &
          & color_map,   &
          & show_progress

contains
  !=============================================================================
  function colorize(s,c) result(o)
    !< Bracket a string with text to change its color on a terminal
    character(*),intent(in)::s !< String to colorize
    integer,dimension(3),intent(in)::c ! c in [0,5]
    !< Color to use in [r,g,b] format, where \(r,b,g \in [0,5]\)
    character(:),allocatable::o

    ! character(1),parameter::CR  = achar(13)
    character(1),parameter::ESC = achar(27)
    character(3),parameter::post = '[0m'

    character(:),allocatable::pre

    pre = ESC//'[38;5;'//to_str(36*c(1)+6*c(2)+c(3)+16)//'m'
    o = trim(pre)//s//ESC//post

  end function colorize
  !=============================================================================
  function color_map(v,r) result(c)
    !< Return the color code for colorize based on the coolwarm color map
    real,intent(in)::v
    !! Value to map
    real,dimension(2),intent(in)::r
    !! Range over which to scale the colors
    integer,dimension(3)::c

    integer::s

    if(v<sum(r)/2.0) then
      s = nint((v-r(1))/(sum(r)/2.0-r(1))*5.0)
      c = [s,s,5]
    else
      s = 5-nint((v-sum(r)/2.0)/(r(2)-sum(r)/2.0)*5.0)
      c = [5,s,s]
    end if

  end function color_map
  !=============================================================================
  subroutine show_progress(m,p,ml)
    !! Create a progress bar through successive calls
    character(*),intent(in)::m
    !! Message to display
    real,intent(in)::p
    !! Progress fraction \(p\in[0,1]\), 0 = start progress 1 = complete progress
    integer,intent(in),optional::ml
    !! Message reserve length (used to align long messages)

    real::r
    integer:: mld, n,k
    character(3) :: persent

    write(persent,'(i0)') int(p*100)

    N = 70
    mld = 40
    if(present(ml)) mld = ml

    write(ounit_,'(a)',advance='no') achar(13)//colorize(m//repeat(' ',mld-len(m))//' [',[5,5,0])
    do k=1,N
      r = real(k-1)/real(N-1)
      if(r<=p) then
        write(ounit_,'(1a)',advance='no') colorize('=',color_map(r,[0.0,1.0]))
      else
        write(ounit_,'(1a)',advance='no') colorize(' ',[0,0,0])
      end if
    end do
    write(ounit_,'(a)',advance='no') colorize('] ',[5,5,0]) //colorize(trim(persent)//' %',color_map(p,[0.0,1.0]))
    flush(ounit_)

  end subroutine show_progress

end module face

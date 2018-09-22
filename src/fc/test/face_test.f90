program string_color_test
  use face
  use iso_c_binding
  implicit none
  integer :: n

  print*, colorize('black',[0,0,0])
  print*, colorize('white',[5,5,5])
  print*, colorize('red',[5,0,0])
  print*, colorize('green',[0,5,0])
  print*, colorize('blue',[0,0,5])
  print*, colorize('yello',[5,5,0])
  print*, colorize('cyan',[5,0,5])
  print*, colorize('purple',[0,5,5])

  do n = 0,20
    print*, colorize('corlormap',color_map(v=0.05*n,r=[0.,1.]))
  enddo

  do n = 1,100
    call wait(0.1)
    call show_progress('progress',n/100.0,10)
  enddo
  write(*,*)

contains
  subroutine wait(sec)
    !<Make the thread sleep
    real,intent(in)::sec
    !<Time to sleep in seconds

    integer(c_int)::usec
    integer(c_int)::ret

    interface
      function doSleep(usec) result(e) bind(C,name='usleep')
        use iso_c_binding
        integer(c_int),value::usec
        integer(c_int)::e
      end function doSleep
    end interface

    usec = nint(sec*1.0E6)
    ret  = doSleep(usec)
  end subroutine wait


end program string_color_test

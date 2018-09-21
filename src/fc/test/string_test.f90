program string_test
  use string
  implicit none

  integer, parameter :: is = 1
  integer, parameter :: iv(*) = [1,2,3,4]
  integer, parameter :: im(*,*) = reshape([1,2,3,4],[2,2])

  real,    parameter :: rs = 1.0
  real,    parameter :: rv(*) = [1.,2.,3.,4.]
  real,    parameter :: rm(*,*) = reshape([1.,2.,3.,4.],[2,2])

  logical, parameter :: ls = .TRUE.
  logical, parameter :: lv(*) = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  logical, parameter :: lm(*,*) = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2])

  write(unit=6, fmt=*) 'is = '//to_str(is)
  write(unit=6, fmt=*) 'iv = '//to_str(iv)
  write(unit=6, fmt=*) 'im = '//to_str(im)

  write(unit=6, fmt=*) 'rs = '//to_str(rs)
  write(unit=6, fmt=*) 'rv = '//to_str(rv)
  write(unit=6, fmt=*) 'rm = '//to_str(rm)

  write(unit=6, fmt=*) 'ls = '//to_str(ls)
  write(unit=6, fmt=*) 'lv = '//to_str(lv)
  write(unit=6, fmt=*) 'lm = '//to_str(lm)

end program string_test

program mmread_test
  use matrix_market
  use string
  use blas_interface
  implicit none

  real, allocatable :: a(:,:)
  character(*), parameter :: filename = '../data/matrix.mtx'

  call readmm(a,filename)

  write(*,*) 'a = '//to_str(a)

end program mmread_test

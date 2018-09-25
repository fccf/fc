
program csr_test
  use sparse_matrix
  use string, only:to_str
  implicit none

  type(csr_matrix) :: csr

  real :: m(4,4),v(4),mv(4)

  m = 0.0
  m(1,3) = 3.14
  m(3,1) = 3.14
  v = 1.0

  write(*,*) 'full_to_csr'
  call csr%set(m)
  write(*,*) 'm =  '
  call csr%print(6)

  write(*,*) 'csr_info'
  write(*,*) 'nr = '//to_str(csr%nr)
  write(*,*) 'nc = '//to_str(csr%nc)
  write(*,*) 'nnnz = '//to_str(csr%nnz)
  write(*,*) 'ia = '//to_str(csr%ia)
  write(*,*) 'ja = '//to_str(csr%ja)
  write(*,*) 'a = '//to_str(csr%a)

  mv = csr%mv(v)

  write(*,*) 'mv = '//to_str(mv)

end program csr_test

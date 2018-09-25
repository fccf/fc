program txt_test
  use txt_interface
  implicit none

  type(txt_file) :: tf

  call tf%open('x.txt')

  print*, 'is open : ', tf%is_open()
  print*, 'is exist: ', tf%is_exist()

  call tf%close()

  print*, 'is open : ', tf%is_open()
  print*, 'is exist: ', tf%is_exist()

  call tf%delete()
  print*, 'is exist: ', tf%is_exist()

end program txt_test

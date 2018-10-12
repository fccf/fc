program file_test
  use file
  implicit none

  character(:), allocatable :: filename

  filename = '/x/xx/xxx/xxxx.xx'

  print*, file_ext(filename)
  print*, file_base(filename)
  print*, file_name(filename)
  print*, file_path(filename)


end program file_test

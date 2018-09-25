program vtk_test
  use gmsh_interface, only:msh_file
  use vtk_interface, only:vtk_file
  implicit none

  type(vtk_file) :: vf
  type(msh_file) :: mf
  character(:), allocatable :: file
  real, allocatable :: coo(:,:), data(:)
  integer, allocatable :: eni(:,:)
  integer :: nn, i

  file = '../data/gmsh.msh'

  call mf%init(file)
  coo = mf%get_coo()
  eni = mf%get_eni()
  nn = mf%get_nn()

  allocate(data(nn))

  call random_number(data)

  call vf%write('x.vtk','test', coo,eni, data)

end program vtk_test

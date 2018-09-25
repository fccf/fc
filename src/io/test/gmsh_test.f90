program gmsh_test
  use gmsh_interface, only: msh_file
  use string, only: to_str
  implicit none
  character(:), allocatable :: file
  type(msh_file) :: gmshf
  real, allocatable :: coo(:,:)
  integer, allocatable :: eni(:,:), fni(:,:), er(:), fr(:), nid(:), eid(:)
  integer :: nd, nn, ne, nf, enn, fnn

  file = '../data/gmsh.msh'

  call gmshf%init(file)

  nd = gmshf%get_nd()
  nn = gmshf%get_nn()
  ne = gmshf%get_ne()
  nf = gmshf%get_nf()
  enn = gmshf%get_enn()
  fnn = gmshf%get_fnn()

  coo = gmshf%get_coo()
  eni = gmshf%get_eni()
  fni = gmshf%get_fni()
  er = gmshf%get_er()
  fr = gmshf%get_fr()
  eid = gmshf%get_eid()
  nid = gmshf%get_nid()

  print*, 'nd = '//to_str(nd)
  print*, 'nn = '//to_str(nn)
  print*, 'ne = '//to_str(ne)
  print*, 'nf = '//to_str(nf)
  print*, 'enn = '//to_str(enn)
  print*, 'fnn = '//to_str(fnn)
  print*, 'nid = '//to_str(nid)
  print*, 'eid = '//to_str(eid)
  print*, 'er = '//to_str(er)
  print*, 'fr= '//to_str(fr)
  print*, 'eni = '//to_str(eni)
  print*, 'fni = '//to_str(fni)
  print*, 'coo = '//to_str(coo)

  call gmshf%destory()

end program gmsh_test

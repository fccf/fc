program test_hdf5_ifc

  use, intrinsic:: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
  use hdf5_interface, only: hdf5_file

  implicit none

  type(hdf5_file) :: h5f
  integer :: i1(4),i2(4,4)
  integer :: one = 1
  real    :: ro = 1.0
  real    :: nan,nant
  integer, parameter :: realbits = storage_size(ro)

  integer, allocatable :: i1t(:),i2t(:,:)
  integer :: ng,nn,pn
  real, allocatable :: flux(:,:),fo(:)

  integer :: i
  character(2) :: ic, pnc, cs

  nan = ieee_value(1.0, ieee_quiet_nan)

  write(cs,'(I2)') realbits

  do concurrent (i = 1:size(i1))
    i1(i) = i
  enddo

  i2(1,:) = i1
  do concurrent (i = 1:size(i2,2))
    i2(i,:) = i2(1,:) * i
  enddo

  print *,'real kind bits: ',cs

  print*, 'initialize ...'
  call h5f%initialize()
  call h5f%open_file('test_'//cs//'.h5',status='NEW',action='WRITE')


  call h5f%add('/test/ai1', i1)
  call h5f%get('/test/ai1',i1t)

  if (.not.all(i1==i1t)) error stop 'read does not match write'


  call h5f%open_group('/test/')
  call h5f%add('group3/scalar',one)
  call h5f%add('group3/scalar_real',ro)
  call h5f%close_group()

  call h5f%add('/test/group2/ai2', i2)
  call h5f%get('/test/group2/ai2',i2t)
  if (.not.all(i2==i2t)) error stop 'read does not match write'

  call h5f%add('/test/nan',nan)
  call h5f%get('/test/nan',nant)
  if (.not.ieee_is_nan(nant)) error stop 'not reading NaN'


  call h5f%close_file()

  !> more group

  pn = 5
  nn = 100
  ng = 69
  allocate(flux(nn,ng),fo(nn))
  flux = ro
  write(pnc,'(I2)') pn
  call h5f%open_file('p'//trim(adjustl(pnc))//'_'//cs//'.h5',status='NEW',action='WRITE')
  do i = 1,ng
    write(ic,'(I2)') i
    call h5f%add('/group'//trim(adjustl(ic))//'/flux_node',flux(:ng,i))
  enddo

  call h5f%get('/group1/flux_node',fo)
  if (.not.all(fo(:ng)==flux(:69,1))) error stop 'read does not match write'

  call h5f%close_file()
  
  call h5f%finalize()

end program test_hdf5_ifc

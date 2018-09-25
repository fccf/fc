module matrix_market
  use iso_fortran_env, only: iostat_end
  implicit none

  public :: readmm

  private

contains
  !=============================================================================
  !> read MatrixMarket format matrix
  subroutine readmm(A,file)
    real, allocatable, intent(out) :: A(:,:)
    character(*), intent(in) :: file

    character(128) :: line
    character(:),allocatable :: line_
    integer :: m,n,nr
    integer :: unit,ierr
    integer :: i,j,k

    open(newunit=unit,file = file)

    k = 0
    do
       read(unit,'(a)',iostat=ierr) line
       if(ierr==iostat_end)exit
       !> skip blank line
       line_ = trim(adjustl(line))
       if (line_ == '')cycle
       if (index(line_, '%')    /= 0)  cycle
       k = k+1
       if(k==1) then
         read(line_,*) m,n,nr
         allocate(A(m,n))
         A = 0.0
       else
         read(line_,*) i,j,a(i,j)
       endif
    enddo

    close(unit)

  end subroutine readmm

end module matrix_market

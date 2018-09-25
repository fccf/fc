!> author: FANGCHAO
!  data: 2017/7/12
!
!## description
! This module provide some matrix and vector operation from blas and lapack
!> NOTE:
!> if want use double precision, must predefine **DOUBLEP**

#include "debug.h"
module blas_interface
  use debug
  implicit none

  public :: DM_mult,MD_mult,kron
  public :: mult,mdm
  public :: solve,cholesky
  public :: inv, invert
  public :: evd,evds,svd,rvd
  public :: norm,norm2,det,outer,cross

  private
  !=============================================================================
  !> interface

  interface mult
     module procedure blas_mm, blas_mv
  end interface mult

  interface mdm
     module procedure Mat_Diag_Mat
  end interface mdm

  interface solve
     module procedure solve_single, solve_multiple
  end interface solve

  interface cholesky
     module procedure cholesky_factor
  end interface cholesky

  interface invert
     module procedure invert_matrix
  end interface invert

  interface inv
     module procedure inverse
  end interface inv

  interface evd
     module procedure eigendecomposition
  end interface evd

  interface evds
     module procedure eigendecomposition_symmetric
  end interface evds

  interface rvd
     module procedure riemann_decomp
  end interface rvd

  interface norm2
     module procedure norm2_vector, norm2_tensor
  end interface norm2

  interface outer
     module procedure outer_product
  end interface outer

  interface cross
     module procedure cross_product
  end interface cross

contains
  !=============================================================================
  function DM_mult(d,M) result(R)

    real,intent(in) :: d(:),M(:,:)
    real :: R(size(d,1),size(M,2))

    integer :: m1,n2,i

    m1 = size(d,1)
    n2 = size(M,1)
    assert(m1==n2)

    do i = 1,m1
       R(i,:) = d(i)*M(i,:)
    end do

  end function DM_mult
  !=============================================================================
  function MD_mult(M,d) result(R)

    real,intent(in) :: M(:,:),d(:)
    real :: R(size(M,1),size(d,1))

    integer :: m1,n2,i

    m1 = size(M,2)
    n2 = size(d,1)

    assert(m1==n2)

    do i = 1,m1
       R(:,i) = M(:,i)*d(i)
    end do

  end function MD_mult
  !=============================================================================
  function kron(A,B) result(C)

    real,intent(in) :: A(:,:),B(:,:)
    real :: C(size(A,1)*size(B,1),size(A,2)*size(B,2))

    integer :: n1,m1,n2,m2,i,j

    n1 = size(A,1)
    m1 = size(A,2)
    n2 = size(B,1)
    m2 = size(B,2)

    do j=1,m1
       do i=1,n1
          C((i-1)*n2+1:i*n2,(j-1)*m2+1:j*m2)=A(i,j)*B
       end do
    end do

  end function kron
  !=============================================================================
  !!> Use DGEMM to multiply A * B and get C.
  function blas_mm(A, B) result(C)

    real, dimension(:, :), intent(in) :: A, B
    real, dimension(size(A, 1), size(B, 2)) :: C

    integer :: m,n,k,lda,ldb,ldc
    real    :: alpha,beta
    character(len = 1) :: transa,transb

    transa = 'N'
    transb = 'N'
    alpha  = 1.0
    beta   = 0.0

    m = size(A,1)
    n = size(B,2)
    k = size(A,2)
    lda = max(1,m)
    ldb = max(1,k)
    ldc = max(1,m)


    assert(size(A, 2) == size(B, 1))

#ifdef DOUBLEP
    call dgemm('N','N',m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
#else
    call sgemm('N','N',m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
#endif

  end function blas_mm
  !=============================================================================
  !!> Use DGEMV to multiply A*x to get b.
  function blas_mv(A, x) result(y)

    real, dimension(:, :), intent(in) :: A
    real, dimension(size(A, 2)), intent(in) :: x
    real, dimension(size(A, 1)) :: y

    integer :: m,n,lda,incx,incy
    real :: alpha,beta
    character(1) :: trans

    trans = 'N'
    m = size(A,1)
    n = size(A,2)
    lda = max(1,m)
    alpha = 1.0
    beta  = 0.0
    incx  = 1
    incy  = 1

#ifdef DOUBLEP
    call dgemv(trans, m, n, alpha, A, lda, x, incx, beta, y, incy)
#else
    call sgemv(trans, m, n, alpha, A, lda, x, incx, beta, y, incy)
#endif

  end function blas_mv
  !=============================================================================
  !!> Construct matrix * diag(diag) * matrix^T
  function Mat_Diag_Mat(matrix, diag)

    real, dimension(:,:), intent(in) :: matrix
    real, dimension(size(matrix,2)) :: diag

    real, dimension(size(matrix,1), size(matrix,2)) :: mat_diag_mat

    mat_diag_mat=&
         mult(MD_mult(matrix,diag),transpose(matrix))

  end function Mat_Diag_Mat
  !=============================================================================
  !!> Solve Ax=b for multiple right hand sides B putting the result in B.
  subroutine solve_multiple(A,B, stat)

    real, intent(in) :: A(:,:)
    real, intent(inout) :: B(:,:)
    integer, optional, intent(out) :: stat

    real    :: At(size(A,1), size(A,2))
    integer :: ipiv(size(A,1))
    integer :: info

    if (present(stat)) stat = 0

    assert(size(A,1)==size(A,2))
    assert(size(A,1)==size(B,1))

    At=A
#ifdef DOUBLEP
    call dgesv(size(A,1), size(B,2), At, size(A,1), ipiv, B, size(B,1),&
         & info)
#else
    call sgesv(size(A,1), size(B,2), At, size(A,1), ipiv, B, size(B,1),&
         & info)
#endif

    if (.not. present(stat)) then
       assert(info==0)
    else
       stat = info
    end if

  end subroutine solve_multiple
  !=============================================================================
  !!> Solve Ax=b for one right hand side b, putting the result in b.
  subroutine solve_single(A, b, info)

    real, dimension(:, :), intent(in) :: A
    real, dimension(:), intent(inout) :: b
    integer, optional, intent(out) :: info

    real, dimension(size(b), 1) :: b_tmp

    b_tmp(:, 1) = b
    call solve_multiple(A, b_tmp, info)
    b = b_tmp(:, 1)

  end subroutine solve_single
  !=============================================================================
  !!> Replace the matrix A with an Upper triangular factor such that
  !!> U^TU=A
  subroutine cholesky_factor(A)

    real, dimension(:,:), intent(inout) :: A

    integer :: info

    integer :: i,j

    assert(size(A,1)==size(A,2))

    ! Zero lower triangular entries.
    forall(i=1:size(A,1),j=1:size(a,2),j<i)
       A(i,j)=0.0
    end forall

#ifdef DOUBLEP
    call dpotrf('U', size(A,1), A, size(A,1), info)
#else
    call spotrf('U', size(A,1), A, size(A,1), info)
#endif

    assert(info==0)

  end subroutine cholesky_factor
  !=============================================================================
  subroutine invert_matrix(A, stat)
    !!< Replace the matrix A with its inverse.
    real, dimension(:,:), intent(inout) :: A
    real, dimension(size(A,1),size(A,2)) :: rhs
    integer, intent(out), optional :: stat

    real, dimension(3,3):: a33
    real det, tmp
    integer i

    assert(size(A,1)==size(A,2))

    if(present(stat)) stat=0

    select case (size(A,1))
    case (3) ! I put this one first in the hope the compiler keeps it there
       det=A(1,1)*(A(2,2)*A(3,3)-A(3,2)*A(2,3)) &
            -A(2,1)*(A(1,2)*A(3,3)-A(3,2)*A(1,3)) &
            +A(3,1)*(A(1,2)*A(2,3)-A(2,2)*A(1,3))

       a33(1,1)=A(2,2)*A(3,3)-A(3,2)*A(2,3)
       a33(1,2)=A(3,2)*A(1,3)-A(1,2)*A(3,3)
       a33(1,3)=A(1,2)*A(2,3)-A(2,2)*A(1,3)

       a33(2,1)=A(2,3)*A(3,1)-A(3,3)*A(2,1)
       a33(2,2)=A(3,3)*A(1,1)-A(1,3)*A(3,1)
       a33(2,3)=A(1,3)*A(2,1)-A(2,3)*A(1,1)

       a33(3,1)=A(2,1)*A(3,2)-A(3,1)*A(2,2)
       a33(3,2)=A(3,1)*A(1,2)-A(1,1)*A(3,2)
       a33(3,3)=A(1,1)*A(2,2)-A(2,1)*A(1,2)

       A=a33/det

    case (2)
       det=A(1,1)*A(2,2)-A(1,2)*A(2,1)
       tmp=A(1,1)
       A(1,1)=A(2,2)
       A(2,2)=tmp
       A(1,2)=-A(1,2)
       A(2,1)=-A(2,1)
       A=A/det

    case (1)
       A(1,1)=1.0/A(1,1)

    case default ! otherwise use LAPACK
       rhs=0.0

       forall(i=1:size(A,1))
          rhs(i,i)=1.0
       end forall

       call solve(A, rhs, stat)

       A=rhs
    end select

  end subroutine invert_matrix
  !=============================================================================
  !!> Function version of invert.
  function inverse(A)

    real, dimension(:,:), intent(in) :: A
    real, dimension(size(A,1),size(A,2)) :: inverse

    real det
    integer i

    assert(size(A,1)==size(A,2))

    select case (size(A,1))
    case (3)
       ! I put this one first in the hope the compiler keeps it there
       det=A(1,1)*(A(2,2)*A(3,3)-A(3,2)*A(2,3)) &
            -A(2,1)*(A(1,2)*A(3,3)-A(3,2)*A(1,3)) &
            +A(3,1)*(A(1,2)*A(2,3)-A(2,2)*A(1,3))

       inverse(1,1)=A(2,2)*A(3,3)-A(3,2)*A(2,3)
       inverse(1,2)=A(3,2)*A(1,3)-A(1,2)*A(3,3)
       inverse(1,3)=A(1,2)*A(2,3)-A(2,2)*A(1,3)

       inverse(2,1)=A(2,3)*A(3,1)-A(3,3)*A(2,1)
       inverse(2,2)=A(3,3)*A(1,1)-A(1,3)*A(3,1)
       inverse(2,3)=A(1,3)*A(2,1)-A(2,3)*A(1,1)

       inverse(3,1)=A(2,1)*A(3,2)-A(3,1)*A(2,2)
       inverse(3,2)=A(3,1)*A(1,2)-A(1,1)*A(3,2)
       inverse(3,3)=A(1,1)*A(2,2)-A(2,1)*A(1,2)

       inverse=inverse/det

    case (2)
       det=A(1,1)*A(2,2)-A(1,2)*A(2,1)
       inverse(1,1)=A(2,2)
       inverse(2,2)=A(1,1)
       inverse(1,2)=-A(1,2)
       inverse(2,1)=-A(2,1)
       inverse=inverse/det

    case (1)
       inverse(1,1)=1.0/A(1,1)

    case default
       inverse=0.0

       forall(i=1:size(A,1))
          inverse(i,i)=1.0
       end forall

       call solve(A, inverse)

    end select

  end function inverse
  !=============================================================================
  !!>  M == matrix to decompose
  !!>  V == matrix whose columns are normalised eigenvectors
  !!>  A == vector of eigenvalues. Assumed to be real
  subroutine eigendecomposition(M, V, A)

    real, dimension(:, :), intent(in) :: M
    real, dimension(:, :), intent(out) :: V
    real, dimension(:), intent(out) :: A

    integer :: dim, lwork
    real, dimension(size(A), size(A)) :: M_temp
    real, dimension(:), allocatable :: work, A_temp
    real :: rdummy(1,1)
    integer :: info

    assert(size(A) .eq. size(M, 1))
    assert(size(A) .eq. size(V, 2))

    dim = size(A)

    lwork =  50 * dim
    allocate(A_temp(dim))
    allocate(work(lwork))

    M_temp = M

#ifdef DOUBLEP
    call DGEEV('N', 'V', dim, M_temp, dim, A, A_temp, rdummy, 1, V, &
         dim, work, lwork, info)
#else
    call SGEEV('N', 'V', dim, M_temp, dim, A, A_temp, rdummy, 1, V, &
         dim, work, lwork, info)
#endif

    assert(info == 0)

    deallocate(work, A_temp)

  end subroutine eigendecomposition
  !=============================================================================
  !!>  M == matrix to decompose
  !!>  V == matrix whose columns are normalised eigenvectors
  !!>  A == vector of eigenvalues. Assumed to be real,
  subroutine eigendecomposition_symmetric(M, V, A, stat)

    real, dimension(:, :), intent(in) :: M
    real, dimension(:, :), intent(out) :: V
    real, dimension(:), intent(out) :: A
    integer, optional, intent(out) :: stat

    integer :: info, i, j, dim
    real, dimension(3 * size(M,1)) :: work
    real, dimension(size(M,1) * (size(M, 1)+1)/2) :: AP

    if(present(stat)) stat = 0

    dim = size(M, 1)
    do j=1,dim
       do i=1,j
          AP(i + (j-1)*j/2) = M(i,j)
       end do
    end do

#ifdef DOUBLEP
    call DSPEV('V', 'U', size(M, 1), AP, A, V, size(V, 1), work, info)
#else
    call SSPEV('V', 'U', size(M, 1), AP, A, V, size(V, 1), work, info)
#endif

    if(info /= 0) then
       if(present(stat)) then
          stat = info
       else
          write(6,*) "For matrix: ", transpose(M)
          error("eigendecomposition_symmetric failed")
       end if
    end if

  end subroutine eigendecomposition_symmetric
  !=============================================================================
  !!> Recompose the matrix M from its eigendecomposition.
  subroutine eigenrecomposition(M, V, A)

    real, dimension(:, :), intent(out) :: M
    real, dimension(:, :), intent(in) :: V
    real, dimension(:), intent(in) :: A
    integer :: i

    do i=1,size(A)
       M(:, i) = A(i) * V(:, i)
    end do

    M = mult(M, transpose(V))

  end subroutine eigenrecomposition
  !=============================================================================
  subroutine svd(input, U, sigma, VT)

    real, dimension(:, :), intent(in) :: input
    real, dimension(size(input, 1), size(input, 1)), intent(out) :: U
    real, dimension(min(size(input, 1), size(input, 2))), intent(out) :: sigma
    real, dimension(size(input, 2), size(input, 2)), intent(out) :: VT

    real, dimension(size(input, 1), size(input, 2)) :: tmp_input
    real, dimension(:), allocatable :: WORK
    integer :: LWORK, M, N, info

    tmp_input = input
    M = size(input, 1)
    N = size(input, 2)
    LWORK = 2*MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N))
    allocate(WORK(LWORK))

#ifdef DOUBLEP
    call DGESVD('A', 'A', M, N, tmp_input, M, &
         sigma, U, M, VT, N, WORK, LWORK, info)
#else
    call SGESVD('A', 'A', M, N, tmp_input, M, &
         sigma, U, M, VT, N, WORK, LWORK, info)
#endif

    assert(info == 0)
    deallocate(WORK)

  end subroutine svd
  !=============================================================================
  subroutine riemann_decomp(A,Aout,Ain)

    real,intent(inout) :: A(:,:)

    real,allocatable,intent(out)          :: Aout(:,:)
    real,allocatable,intent(out),optional :: Ain(:,:)

    real :: v(size(A,1)),vin(size(A,1)),vout(size(A,1)),em(size(A,1),size(A,1))
    real :: emt(size(A,1),size(A,1))
    integer :: n,m,i


    n = size(A,1)
    m = size(A,2)

    if(n /= m) then
       error stop 'Matrix must be symmetry!'
    end if

    allocate(Aout(n,n))

    vin  = 0.0d0
    vout = 0.0d0

    call evds(a, em, v)

    do i = 1,n
       if(v(i)>=0) then
          vout(i) = v(i)
       else
          vin(i) = v(i)
       end if
    end do

    emt = transpose(em)
    Aout = mult(MD_mult(em,vout),emt)

    if(present(Ain)) then
       allocate(Ain(n,n))
       Ain  = mult(MD_mult(em,vin),emt)
    end if

  end subroutine riemann_decomp
  !=============================================================================
  subroutine norm(vec)

    real,intent(in out) :: vec(:)
    integer :: n,i
    real    :: det

    n = size(vec,1)
    det = 0.0d0

    do i = 1,n
       det = det + vec(i)*vec(i)
    end do

    vec = vec/sqrt(det)

  end subroutine norm
  !=============================================================================
  !!> Calculate the 2-norm of vector
  pure function norm2_vector(vector)

    real :: norm2_vector
    real, dimension(:), intent(in) :: vector

    norm2_vector=sqrt(dot_product(vector, vector))

  end function norm2_vector
  !=============================================================================
  !!> Calculate the 2-norm of tensor
  pure function norm2_tensor(tensor)

    real :: norm2_tensor
    real, dimension(:,:), intent(in) :: tensor

    norm2_tensor=sqrt(sum(tensor(:,:)*tensor(:,:)))

  end function norm2_tensor
  !=============================================================================
  pure function outer_product(x, y)

    real, dimension(:), intent(in) :: x, y
    real, dimension(size(x), size(y)) :: outer_product
    integer :: i, j

    forall (i=1:size(x))
       forall (j=1:size(y))
          outer_product(i, j) = x(i) * y(j)
       end forall
    end forall

  end function outer_product
  !=============================================================================
  !!> Calculate the cross product of the vectors provided.
  pure function cross_product(vector1, vector2) result(prod)

    real, dimension(3) :: prod
    real, dimension(3), intent(in) :: vector1, vector2

    prod(1)=vector1(2)*vector2(3) - vector1(3)*vector2(2)
    prod(2)=vector1(3)*vector2(1) - vector1(1)*vector2(3)
    prod(3)=vector1(1)*vector2(2) - vector1(2)*vector2(1)

  end function cross_product
  !=============================================================================
  !!> Determinate of matrix
  function det(mat) result(det_out)

    real, dimension(:, :), intent(in) :: mat
    real :: det_out

    det_out = huge(0.0)

    select case (size(mat,1))
    case(1)
       det_out = mat(1,1)
    case(2)
       det_out = (mat(1,1)*mat(2,2))-(mat(1,2)*mat(2,1))
    case(3)
       det_out = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) &
            - mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) &
            + mat(1,3)*(mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1))
    case default
       error("Determinant not implemented for this dimension")
    end select

  end function det
  !<
end module blas_interface


module sparse_solver
  use sparse_matrix

  implicit none

  public :: iter_solver

  private

  type :: iter_solver

    integer :: itmax   !< max iteration number
    integer :: irst    !< restart number
    real    :: eps
    integer :: itrace   !< if print the iteration info? = 1, print ,0 not

    integer :: iter_used
    real    :: final_res
  contains

    procedure :: set => set_parameter
    procedure :: solve
    procedure :: print_summary

  end type iter_solver


contains
  !=============================================================================
  subroutine set_parameter(this,itmax, irst, eps, itrace)

    class(iter_solver),intent(out) :: this
    integer, intent(in) :: itmax, irst, itrace
    real, intent(in) :: eps

    this%itmax = itmax
    this%irst = irst
    this%eps = eps
    this%itrace = itrace

    this%iter_used = 0
    this%final_res = 0.0

  end subroutine set_parameter
  !=============================================================================
  !> applies the preconditioned restarted GMRES algorithm.
  subroutine solve(this,csr,rhs,x)

    class(iter_solver), intent(inout) :: this
    class(csr_matrix),  intent(inout) :: csr
    real, intent(in) :: rhs(:)
    real, intent(out):: x(:)

    real :: y(this%irst+1)
    real :: v(csr%nr,this%irst+1)
    real :: r(csr%nr)
    real :: g(this%irst+1)
    real :: h(this%irst+1,this%irst)
    real :: c(this%irst+1),s(this%irst+1)
    real :: l(csr%nnz)
    integer :: ua(csr%nr)

    integer :: itr, itr_out, itr_used, i, j, k, k_copy
    real :: mu,rho,rho_tol
    real :: av, htmp
    real, parameter :: delta   = 1.0e-03

    associate(mr => this%irst,itmax => this%itmax ,eps => this%eps,&
      a=>csr%a,ia=>csr%ia,ja=>csr%ja,n=>csr%nr,&
      nz_num => csr%nnz)

      itr_used = 0

      call csr%sort()

      call csr%find_diag(ua)

      call csr%ilu_fact(ua,l)

      itr = 0
      itr_out = 0
      !> outer iter_solver
      do
        itr_out = itr_out + 1
        if(itr> itmax) exit
        if(this%itrace == 1) then
          write(*,*) 'restart: ',itr_out
        endif
        !> r = Ax
        r = csr%mv(x)

        !> r0 = b - Ax
        r(1:n) = rhs(1:n) - r(1:n)

        call csr%ilu_pre( l, ua, r, r )

        !> rho = ||r0||
        rho = sqrt ( dot_product ( r, r ) )




        if ( itr_out == 1 ) then
          rho_tol = rho * eps
        end if

        !> v1 = r0/||r0||
        v(1:n,1) = r(1:n) / rho

        g(1) = rho
        g(2:mr+1) = 0.0

        h(1:mr+1,1:mr) = 0.0

        !> inner iter_solver, restart parameter m = mr
        do k = 1, mr

          itr = itr + 1

          k_copy = k

          !> v(:,k+1)= Av(:,k)
          v(1:n,k+1) = csr%mv(v(1:n,k))

          call csr%ilu_pre ( l, ua, v(1:n,k+1), v(1:n,k+1) )

          av = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )

          !> Arnoldi procedure
          do j = 1, k
            h(j,k) = dot_product ( v(1:n,k+1), v(1:n,j) )
            v(1:n,k+1) = v(1:n,k+1) - v(1:n,j) * h(j,k)
          end do

          h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )

          if ( ( av + delta * h(k+1,k)) == av ) then
            do j = 1, k
              htmp = dot_product ( v(1:n,k+1), v(1:n,j) )
              h(j,k) = h(j,k) + htmp
              v(1:n,k+1) = v(1:n,k+1) - htmp * v(1:n,j)
            end do
            h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
          end if

          if ( h(k+1,k) /= 0.0 ) then
            v(1:n,k+1) = v(1:n,k+1) / h(k+1,k)
          end if

          !> givens transformation for Hessenberg matrix h
          if ( 1 < k ) then
            y(1:k+1) = h(1:k+1,k)
            do j = 1, k - 1
              call mult_givens ( c(j), s(j), j, y )
            end do
            h(1:k+1,k) = y(1:k+1)
          end if

          mu = sqrt ( h(k,k)**2 + h(k+1,k)**2 )

          c(k) = h(k,k) / mu
          s(k) = -h(k+1,k) / mu
          h(k,k) = c(k) * h(k,k) - s(k) * h(k+1,k)
          h(k+1,k) = 0.0
          call mult_givens ( c(k), s(k), k, g )

          rho = abs ( g(k+1) )

          itr_used = itr_used + 1


          if(this%itrace == 1) write(*,'(i6,2x,es15.6)') k, rho


          if ( rho <= eps) exit


        end do  ! end inner iter_solver

        k = k_copy - 1

        !> back substitute for y
        y(k+1) = g(k+1) / h(k+1,k+1)

        do i = k, 1, -1
          y(i) = ( g(i) - dot_product(h(i,i+1:k+1),y(i+1:k+1)))/h(i,i)
        end do

        do i = 1, n

          !> x = x+z  = x0+v*y
          x(i) = x(i) + dot_product ( v(i,1:k+1), y(1:k+1) )

        end do

        if ( rho <= eps ) exit

      end do  ! end outter iter_solver

      this%iter_used   = itr_used
      this%final_res   = rho

    end associate

  end subroutine solve
  !=============================================================================
  !> applies a Givens rotation to two successive entries of a vector.
  subroutine mult_givens(c, s, k, g)

    real, intent(in)    :: c,s
    integer, intent(in)    :: k
    real, intent(inout) :: g(1:k+1)

    real :: g1,g2

    g1 = c * g(k) - s * g(k+1)
    g2 = s * g(k) + c * g(k+1)

    g(k)   = g1
    g(k+1) = g2

  end subroutine mult_givens
  !=============================================================================
  subroutine print_summary(this,unit)

    class(iter_solver), intent(in) :: this
    integer, intent(in) :: unit

    write(unit,'(a)')        ' GMRES SOLVER SUMMARY'
    write(unit,'(a,i15)')    '  Iteration used          = ', this%iter_used
    write(unit,'(a,es15.7)') '  Final residual          = ', this%final_res

  end subroutine print_summary
end module sparse_solver

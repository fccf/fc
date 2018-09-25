module sparse_matrix

  implicit none

  public :: csr_matrix, coo_matrix

  private

  !=============================================================================
  type csr_matrix
    integer :: nr  !< number of rows
    integer :: nc  !< number of columns
    integer :: nnz !< number of none zeros

    real, allocatable :: a(:)     !< none-zeros value(nnz)
    integer, allocatable :: ia(:) !< first none-zeros element from rows (nr+1)
    integer, allocatable :: ja(:) !< column indices for none-zeros (nnz)
  contains
    procedure :: destory => csr_destory
    generic   :: set     => set_csr, &     !< set from nr, nc, nnz, a, ia, ja
                          & full_to_csr, & !< trans a full matrix to xsr
                            allocate_csr   !< allocate csr

    procedure :: to_full => csr_to_full
    procedure :: at
    procedure :: sort
    procedure :: find_diag
    procedure :: print
    procedure :: mv  => mult_vector
    procedure :: tmv => trans_mult_vector
    procedure :: nnz_csr_add
    procedure :: nnz_csr_mult
    procedure :: mult_csr
    procedure :: add_csr
    procedure :: ilu_fact
    procedure :: ilu_pre

    procedure,private :: allocate_csr
    procedure,private :: set_csr
    procedure,private :: full_to_csr
    final :: csr_final

  end type csr_matrix
  !=============================================================================
  type coo_matrix
    integer :: nr !< number rows
    integer :: nc !< number cols
    integer :: nnz  = 0!< number of none zeros
    integer, allocatable :: rows(:) !< row index
    integer, allocatable :: cols(:) !< coloum index
    real, allocatable    :: vals(:) !< element value
  contains
    procedure :: to_full => coo_to_full
    procedure :: to_null => coo_to_null
    procedure :: set     => full_to_coo
  end type coo_matrix

contains
  !=============================================================================
  pure function coo_to_full(this) result(full)
    class(coo_matrix), intent(in) :: this
    real :: full(this%nr,this%nc)
    integer :: i, j, k

    full = 0.0
    do k = 1,this%nnz
      i = this%rows(k)
      j = this%cols(k)
      full(i,j) = this%vals(k)
    end do

  end function coo_to_full
  !=============================================================================
  subroutine coo_to_null(this)
    class(coo_matrix), intent(inout) :: this
    this%nr = 0
    this%nc = 0
    this%nnz = 0
    this%rows = [integer :: ]
    this%cols = [integer :: ]
    this%vals = [real :: ]
  end subroutine coo_to_null
  !=============================================================================
  subroutine full_to_coo(coo, full, eps)
    class(coo_matrix), intent(inout) :: coo
    real, intent(in) :: full(:,:)
    real, intent(in), optional :: eps

    integer :: i, j
    real :: eps_zero

    eps_zero = 1.0e-15
    if(present(eps)) eps_zero = eps

    call coo%to_null()
    coo%nr = size(full,1)
    coo%nc = size(full,2)

    do j = 1,coo%nc
      do i = 1,coo%nr
        if(abs(full(i,j)) > eps_zero) then
          coo%rows = [coo%rows,i]
          coo%cols = [coo%cols,j]
          coo%vals = [coo%vals,full(i,j)]
        endif
      enddo
    enddo

    coo%nnz = size(coo%vals)

  end subroutine full_to_coo

  !=============================================================================
  !> allocate the array
  subroutine allocate_csr(this,nr,nc,nnz)

    class(csr_matrix),intent(out) :: this
    integer,intent(in) :: nr,nc,nnz

    allocate(this%a(nnz))
    allocate(this%ja(nnz))
    allocate(this%ia(nr+1))

    this%nr  = nr
    this%nc  = nc
    this%nnz = nnz

    this%a  = 0.0
    this%ja = 0
    this%ia = 0

  end subroutine allocate_csr
  !=============================================================================
  subroutine set_csr(this,nr,nc,nnz,ia,ja,a)
    class(csr_matrix), intent(inout) :: this
    integer, intent(in) :: nr,nc,nnz
    integer, intent(in) :: ia(:),ja(:)
    real, intent(in)    :: a(:)

    allocate(this%a(nnz))
    allocate(this%ja(nnz))
    allocate(this%ia(nr+1))

    this%nr  = nr
    this%nc  = nc
    this%nnz = nnz
    this%a   = a
    this%ia  = ia
    this%ja  = ja

  end subroutine set_csr
  !=============================================================================
  subroutine full_to_csr(this,m,eps)
    class(csr_matrix), intent(inout) :: this
    real, intent(in) :: m(:,:)
    real, intent(in), optional :: eps
    integer :: i,j,k
    integer :: nr,nc,nnz
    integer,allocatable:: ia(:),ja(:)
    real, allocatable  :: a(:)
    real :: eps_zero

    eps_zero = 1.0e-8
    if(present(eps)) eps_zero = eps

    nr = size(m,1)
    nc = size(m,2)

    allocate(ia(nr+1))

    ia = 0
    do i = 1,nr
      do j = 1,nc
        if(abs(m(i,j)) > eps_zero) then
          ia(i+1) = ia(i+1) + 1
        endif
      enddo
    enddo

    ia(1) = 1
    do i = 2,nr+1
      ia(i) = ia(i-1) + ia(i)
    enddo

    nnz = ia(nr+1) - 1

    allocate(a(nnz),ja(nnz))
    k = 0
    do i = 1,nr
      do j = 1,nc
        if(abs(m(i,j)) > eps_zero) then
          k = k+1
          a(k) = m(i,j)
          ja(k) = j
        endif
      enddo
    enddo

    call this%set(nr,nc,nnz,ia,ja,a)


  end subroutine full_to_csr
  !=============================================================================
  !> print a csr matrix in full matrix format
  subroutine print(this,unit)

    class(csr_matrix),intent(in) :: this
    integer,intent(in) :: unit

    integer :: i,j

    do i = 1,this%nr
      write(unit,'(*(es15.7))') (this%at(i,j),j=1,this%nc)
    enddo

  end subroutine print
  !=============================================================================
  subroutine csr_to_full(this,m)
    class(csr_matrix), intent(in) :: this
    real, allocatable, intent(out):: m(:,:)
    integer :: i,j,k

    allocate(m(this%nr,this%nc))
    m = 0.0

    do i = 1,this%nr
      do k = this%ia(i),this%ia(i+1)-1
        j = this%ja(k)
        m(i,j) = this%a(k)
      enddo
    enddo

  end subroutine csr_to_full
  !=============================================================================
  !> the value at (i,j)
  function at(this,i,j) result(res)

    class(csr_matrix),intent(in) :: this
    integer,intent(in) :: i,j

    real :: res

    integer :: k
    integer :: iadd = 0

    res = 0.0

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,&
         ibeg => this%ia(i),iend => this%ia(i+1) - 1)
      do k = ibeg,iend
         if(ja(k)== j) then
            iadd = k
            if(iadd /= 0) then
               res = a(iadd)
            end if
            return
         end if
      end do
    end associate

  end function at
  !=============================================================================
  !> finds diagonal entries in a sparse compressed row matrix.
  subroutine find_diag(this, res)

    class(csr_matrix),intent(in) :: this
    integer, intent(out) :: res(this%nr)

    integer :: i,k

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr)

      res = -1

      do i = 1,n
         do k = ia(i),ia(i+1)-1
            if(ja(k) == i) then
               res(i) = k
            end if

         end do
      end do

    end associate

  end subroutine find_diag
  !=============================================================================
  !> sorts a csr matrix.
  subroutine sort(this)

    class(csr_matrix),intent(inout) :: this

    integer :: i,k,l,i4temp
    real :: r8temp

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr)

      do i = 1, n

         do k = ia(i), ia(i+1) - 2
            do l = k + 1, ia(i+1) - 1

               if ( ja(l) < ja(k) ) then
                  i4temp = ja(l)
                  ja(l)  = ja(k)
                  ja(k)  = i4temp

                  r8temp = a(l)
                  a(l)   = a(k)
                  a(k)   = r8temp
               end if

            end do
         end do

      end do

    end associate

  end subroutine sort
  !=============================================================================
  !> computes A*x for a matrix stored in csr format.
  function mult_vector(this,x) result(y)

    class(csr_matrix),intent(in) :: this
    real,intent(in) :: x(:)
    real :: y(size(x,1))

    real :: t
    integer :: i,k


    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr)


      do i = 1, n

         !> Compute the inner product of row I with vector X.
         t = 0.0d0
         do k = ia(i), ia(i+1)-1
            t = t + a(k) * x(ja(k))
         end do

         y(i) = t

      end do

    end associate

  end function mult_vector
  !=============================================================================
  !> computes A'*x for a matrix stored in csr format.
  function trans_mult_vector(this,x) result(y)

    class(csr_matrix),intent(in) :: this
    real,intent(in) :: x(:)
    real :: y(size(x,1))

    integer :: i,k1,k2

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr)

      y = 0.0d0
      do i = 1, n

         k1 = ia(i)
         k2 = ia(i+1) - 1
         y(ja(k1:k2)) = y(ja(k1:k2)) + a(k1:k2) * x(i)

      end do

    end associate

  end function trans_mult_vector
  !=============================================================================
  !> number of none-zero element of 2 csr matrix add
  function nnz_csr_add(this,that) result(nnz)

    class(csr_matrix),intent(in) :: this,that
    integer :: nnz

    integer :: i,j,k,jr,jc
    integer :: ldg,last

    integer :: iw(this%nc)
    integer :: ndegr(this%nr)

    if(this%nr /= that%nr .or. this%nc /= that%nc) then

        error stop 'error: nnz_csr_add, imension mismatch!'

    end if

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,&
         b=>that%a,ib=>that%ia,jb=>that%ja,&
         nrow=>this%nr,ncol=>this%nc)

      iw(1:ncol)    = 0
      ndegr(1:nrow) = 0

      do i = 1, nrow

         ldg = 0

         !> End-of-linked list.
         last = -1

         !>  Row of A.
         do j = ia(i), ia(i+1)-1
            jr = ja(j)

            !>  Add element to the linked list.
            ldg = ldg + 1
            iw(jr) = last
            last = jr
         end do

         !> Row of B.
         do j = ib(i), ib(i+1)-1

            jc = jb(j)

            !> Add one element to the linked list.
            if ( iw(jc) == 0 ) then
               ldg = ldg + 1
               iw(jc) = last
               last = jc
            end if

         end do

         !>  Done with row I.
         ndegr(i) = ldg

         !> Reset IW to zero.
         do k = 1, ldg
            j = iw(last)
            iw(last) = 0
            last = j
         end do

      end do

      nnz = sum ( ndegr(1:nrow) )

    end associate

  end function nnz_csr_add
  !=============================================================================
  !> number of none-zero element of 2 csr matrix multiply
  function nnz_csr_mult(this,that) result(nnz)

    class(csr_matrix),intent(in) :: this,that
    integer :: nnz

    integer :: ii,j,k,jr,jc
    integer :: last,ldg
    integer :: iw(that%nc),ndegr(this%nr)

    if(this%nc /= that%nr) then

       error stop 'mult_csr,dimension mismatch!'

    end if

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,&
         b=>that%a,ib=>that%ia,jb=>that%ja,&
         nrow=>this%nr,ncolb=>that%nc)

      iw(1:ncolb)   = 0
      ndegr(1:nrow) = 0

      do ii = 1, nrow

         !> For each row of A.
         ldg = 0

         !> End-of-linked list.
         last = -1

         do j = ia(ii), ia(ii+1)-1

            !> Row number to be added.
            jr = ja(j)

            do k = ib(jr), ib(jr+1)-1
               jc = jb(k)

               !>  Add one element to the linked list.
               if ( iw(jc) == 0 ) then
                  ldg = ldg + 1
                  iw(jc) = last
                  last = jc
               end if

            end do

         end do

         ndegr(ii) = ldg

         !>  Reset IW to zero.
         do k = 1, ldg
            j = iw(last)
            iw(last) = 0
            last = j
         end do

      end do

      nnz = sum ( ndegr(1:nrow) )

    end associate

  end function nnz_csr_mult
  !=============================================================================
  !> add 2 csr matrix
  subroutine add_csr(this,that,res)

    class(csr_matrix),intent(in) :: this,that
    class(csr_matrix),intent(out):: res

    logical :: values
    integer :: jcol,jpos
    integer :: ii,k,ka,kb,len,nzmax
    integer :: ierr
    integer :: iw(this%nc)

    if(this%nr /= that%nr .or. this%nc /= that%nc) then

       error stop 'error: add_csr, dimension mismatch!'

    end if

    nzmax = this%nnz_csr_add(that)

    call res%set(this%nr,that%nc,nzmax)

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,&
         b=>that%a,ib=>that%ia,jb=>that%ja,&
         c=>res%a,ic=>res%ia,jc=>res%ja,&
         nrow=>res%nr,ncol=>res%nc)

      values = .true.
      ierr  = 0
      len   = 0
      ic(1) = 1
      iw(1:ncol) = 0

      do ii = 1, nrow

         !> Row I.
         do ka = ia(ii), ia(ii+1)-1

            len = len + 1
            jcol = ja(ka)

            if ( nzmax < len ) then
               ierr = ii
               return
            end if

            jc(len) = jcol
            if ( values ) then
               c(len) = a(ka)
            end if
            iw(jcol) = len
         end do

         do kb = ib(ii), ib(ii+1)-1

            jcol = jb(kb)
            jpos = iw(jcol)

            if ( jpos == 0 ) then

               len = len + 1

               if ( nzmax < len ) then
                  ierr = ii
                  return
               end if

               jc(len) = jcol
               if ( values ) then
                  c(len) = b(kb)
               end if
               iw(jcol)= len
            else
               if ( values ) then
                  c(jpos) = c(jpos) + b(kb)
               end if
            end if

         end do

         do k = ic(ii), len
            iw(jc(k)) = 0
         end do

         ic(ii+1) = len+1
      end do

    end associate

  end subroutine add_csr
  !=============================================================================
  !> multiply 2 csr matrix
  subroutine mult_csr(this,that,res)

    class(csr_matrix),intent(in) :: this,that
    class(csr_matrix),intent(out):: res

    logical :: values
    integer :: jcol,jpos
    integer :: ii,jj,k,ka,kb,len,nzmax
    integer :: ierr
    integer :: iw(this%nc)
    real    :: scal

    nzmax = this%nnz_csr_mult(that)

    call res%set(this%nr,that%nc,nzmax)

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,&
         b=>that%a,ib=>that%ia,jb=>that%ja,&
         c=>res%a,ic=>res%ia,jc=>res%ja,&
         nrow=>res%nr,ncol=>res%nc)

      values = .true.
      len   = 0
      ic(1) = 1
      ierr  = 0

      !> Initialize IW.
      iw(1:ncol) = 0

      do ii = 1, nrow

         !> Row I.
         do ka = ia(ii), ia(ii+1)-1

            if ( values ) then
               scal = a(ka)
            end if

            jj = ja(ka)

            do kb = ib(jj), ib(jj+1)-1

               jcol = jb(kb)
               jpos = iw(jcol)

               if ( jpos == 0 ) then
                  len = len + 1
                  if ( nzmax < len ) then
                     ierr = ii
                     return
                  end if
                  jc(len) = jcol
                  iw(jcol)= len
                  if ( values ) then
                     c(len) = scal * b(kb)
                  end if
               else
                  if ( values ) then
                     c(jpos) = c(jpos) + scal * b(kb)
                  end if
               end if

            end do

         end do

         do k = ic(ii), len
            iw(jc(k)) = 0
         end do

         ic(ii+1) = len + 1

      end do

    end associate

  end subroutine mult_csr
  !=============================================================================
  !>  computes the incomplete LU factorization of a csr matrix.
  subroutine ilu_fact(this,ua,l)

    class(csr_matrix), intent(inout) :: this
    integer, intent(inout):: ua(this%nr)
    real, intent(inout):: l(this%nnz)
    integer :: i,j,k,jj,jrow, jw
    real :: tl
    real, parameter :: eps_zero = 1.0e-8

    integer, allocatable :: iw(:)

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr,&
         nz_num => this%nnz)

      allocate(iw(n))

      l(1:nz_num) = a(1:nz_num)

      do i = 1, n

         !> IW points to the nonzero entries in row I.
         iw(1:n) = -1

         do k = ia(i), ia(i+1) - 1
            iw(ja(k)) = k
         end do

         do j = ia(i), ia(i+1) - 1
            jrow = ja(j)
            if ( i <= jrow ) then
               exit
            end if
            tl = l(j) * l(ua(jrow))
            l(j) = tl
            do jj = ua(jrow) + 1, ia(jrow+1) - 1
               jw = iw(ja(jj))
               if ( jw /= -1 ) then
                  l(jw) = l(jw) - tl * l(jj)
               end if
            end do
         end do

         ua(i) = j

         if ( jrow /= i ) then
           error stop 'jrow /= i'
         end if

         if ( abs(l(j)) < eps_zero ) then

            error stop 'ilu_fact! Zero pivot on step I '

         end if

         l(j) = 1.0 / l(j)

      end do

      l(ua(1:n)) = 1.0 / l(ua(1:n))

    end associate

    deallocate(iw)

  end subroutine ilu_fact
  !==========================================================================
  !> applies the incomplete LU preconditioner.
  subroutine ilu_pre(this, l, ua, rhs, z)

    class(csr_matrix), intent(in) :: this
    integer, intent(in) :: ua(:)
    real, intent(in) :: rhs(:)
    real, intent(in) :: l(:)
    real, intent(inout):: z(:)

    real, allocatable :: w(:)

    integer :: i,j

    associate(a=>this%a,ia=>this%ia,ja=>this%ja,n=>this%nr,&
         nz_num => this%nnz)

      allocate(w(n))

      !>  Copy R in.
      w(1:n) = rhs(1:n)

      !> Solve L * w = w where L is unit lower triangular.
      do i = 2, n
         do j = ia(i), ua(i) - 1
            w(i) = w(i) - l(j) * w(ja(j))
         end do
      end do

      !> Solve U * w = w, where U is upper triangular.
      do i = n, 1, -1
         do j = ua(i) + 1, ia(i+1) - 1
            w(i) = w(i) - l(j) * w(ja(j))
         end do
         w(i) = w(i) / l(ua(i))
      end do

      !> Copy Z out.
      z(1:n) = w(1:n)

    end associate

  end subroutine ilu_pre
  !=============================================================================
  subroutine csr_destory(this)
    class(csr_matrix), intent(inout) :: this

    if(allocated(this%a)) deallocate(this%a)
    if(allocated(this%ia)) deallocate(this%ia)
    if(allocated(this%ja)) deallocate(this%ja)

    this%nr = 0
    this%nc = 0
    this%nnz = 0

  end subroutine csr_destory
  !=============================================================================
  subroutine csr_final(this)
    type(csr_matrix), intent(inout) :: this
    call this%destory()
  end subroutine csr_final

end module sparse_matrix

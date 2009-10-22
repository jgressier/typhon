!------------------------------------------------------------------------------!
! Procedure : bdlu_gmres                        Authors : G. Grondin
!
! Function
!   Resolution of linear system : mat.sol = rhs
!     mat type(st_bdlu)
!     Generalized Minimum Residual method (GMRES)
!
! Defaults/Limitations/Misc :
!   - Array sol(*) contains rhs as input
!
!------------------------------------------------------------------------------!
subroutine bdlu_gmres(def_impli, mat, sol, info)

use TYPHMAKE
use SPARSE_MAT
use LAPACK
use MENU_NUM

implicit none

! -- Inputs --
type(mnu_imp) :: def_impli
type(st_bdlu) :: mat

! -- Inputs/Outputs --
real(krp)     :: sol(1:mat%dim*mat%dimblock)  ! RHS as input, SOLUTION as output

! -- Outputs --
integer(kip)  :: info

! -- Internal variables --
real(krp), dimension(:),   allocatable :: qc, qs, ss, yy
real(krp), dimension(:,:), allocatable :: hh
real(krp), dimension(:),   allocatable :: r1, p1
real(krp), dimension(:,:), allocatable :: w1, v1

integer(kip)                         :: nit, dim
real(krp)                            :: errgmres, ref
real(krp)                            :: beta, fact, tmp, normp1

integer(kip)                         :: ik, jk, nk, i, ib, is
integer(kip)                         :: nkrylov

!!! integer :: debug_flag
!!! common /debug/ debug_flag

! -- Body --

nkrylov = 10_kip

dim = mat%dim*mat%dimblock

! initialisation

nit    = 0
errgmres = huge(errgmres)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate(qc(nkrylov))
allocate(qs(nkrylov))
allocate(ss(nkrylov+1))
allocate(yy(nkrylov))
allocate(hh(nkrylov+1,nkrylov))

allocate(r1(dim))
allocate(p1(dim))

allocate(w1(nkrylov,dim))
allocate(v1(nkrylov,dim))

! -- initialization --

p1 (1:dim) = sol(1:dim)  ! save RHS

! -- initial guess --

do i = 1, mat%dim
  is = (i-1)*mat%dimblock
  do ib = 1, mat%dimblock
    sol(is+ib) = p1(is+ib) / mat%diag(ib,ib,i)  ! initial guess
  enddo
enddo
! ref = norme_L1(sol)
ref = sum(abs(sol(1:dim)))

! normp1 = norme_L2(p1)
normp1 = sqrt(dot_product(p1(1:dim),p1(1:dim)))

do while ((errgmres >= ref*def_impli%maxres).and.(nit <= def_impli%max_it*2))

!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,i4)') '  iter gmres ',nit+1
!!! call flush(6)
!!! endif

  ! r0 = M^(-1) . ( b - A.x0 )
  call bdlu_yeqmaxpz(r1(1:dim), mat, sol(1:dim), p1(1:dim))      ! R1 = RHS - MAT.SOL

  beta = sqrt(dot_product(r1(1:dim),r1(1:dim)))                 ! beta = ||r0||_2
  fact = 1.0_krp/beta

  v1(1,1:dim) = fact*r1(1:dim)                                  ! v1 = r0/|r0|

  ss(1:nkrylov+1) = 0.0_krp
  ss(1) = beta                                                  ! s(1) = |r0|

  hh(1:nkrylov+1,1:nkrylov) = 0.0_krp

  do jk = 1,nkrylov

    call bdlu_yeqax (w1(jk,1:dim), mat, v1(jk,1:dim))           ! wj = M^(-1).A.vj

    do ik = 1,jk
      hh(ik,jk) = dot_product(w1(jk,1:dim),v1(ik,1:dim))        ! hij = (wj,vi)
      w1(jk,1:dim) = w1(jk,1:dim) - hh(ik,jk)*v1(ik,1:dim)      ! wj = wj - hij*vi
    enddo

    hh(jk+1,jk) = sqrt(dot_product(w1(jk,1:dim),w1(jk,1:dim)))  ! h(j+1,j) = ||wj||_2

    do ik = 1,jk-1                                            ! apply J(1)...J(kj-1) on hh
      tmp = hh(ik,jk)
      hh(ik  ,jk) =  qc(ik)*tmp + qs(ik)*hh(ik+1,jk)
      hh(ik+1,jk) = -qs(ik)*tmp + qc(ik)*hh(ik+1,jk)
    enddo

    if ( hh(jk+1,jk) > epsilon(normp1)*10*normp1 ) then

      if ( jk < nkrylov ) then
        fact = 1.0_krp/hh(jk+1,jk)
        v1(jk+1,1:dim) = fact*w1(jk,1:dim)                        ! v(j+1) = wj/h(j+1,j)
      endif

      if ( hh(jk,jk) == 0 ) exit                                ! Exit if hh(jk,jk)==0

      tmp = sqrt(hh(jk,jk)**2+hh(jk+1,jk)**2)                   ! build J(jk)
      qc(jk) = hh(jk  ,jk)/tmp
      qs(jk) = hh(jk+1,jk)/tmp

      hh(jk  ,jk) = tmp
      hh(jk+1,jk) = 0.0_krp

      ss(jk+1) = -qs(jk)*ss(jk)                                 ! apply J(jk) on ss
      ss(jk  ) =  qc(jk)*ss(jk)

      if ( abs(ss(jk+1)) < def_impli%maxres*abs(ss(1)) ) exit

!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,2(i2,a),1pe12.5,$)') '  hh[',jk  ,',',jk,']=',hh(jk  ,jk)
!!! write(6,'(a,2(i2,a),1pe12.5,$)') '  hh[',jk+1,',',jk,']=',hh(jk+1,jk)
!!! write(6,'(a,  i2,a ,1pe12.5,$)') '  ss[',         jk,']=',ss(     jk)
!!! write(6,'(a,  i2,a ,1pe12.5,$)') '  qc[',         jk,']=',qc(     jk)
!!! write(6,'(a,  i2,a ,1pe12.5  )') '  qs[',         jk,']=',qs(     jk)
!!! call flush(6)
!!! endif

    else

!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,2(i2,a),1pe12.5  )') '* hh[',jk+1,',',jk,']=',hh(jk+1,jk)
!!! call flush(6)
!!! endif

      exit

    endif

  enddo

  nk = min(jk,nkrylov)

  ! Solve H.y = s
  do jk = nk,1,-1
    tmp = ss(jk)
    do ik = jk+1,nk
      tmp = tmp - hh(jk,ik)*yy(ik)
    enddo
    yy(jk) = tmp/hh(jk,jk)
!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,i2,a,1pe12.5)') '  yy[',jk,'] = ',yy(jk)
!!! endif
    sol(1:dim) = sol(1:dim)+yy(jk)*v1(jk,1:dim)                   ! sol = sol + Vm.ym
  enddo

  call bdlu_yeqmaxpz(r1(1:dim), mat, sol(1:dim), p1(1:dim))       ! R1 = RHS - MAT.SOL
  ss(nk+1) =  sqrt(dot_product(r1(1:dim),r1(1:dim)))

  errgmres = ss(nk+1)/normp1

!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,1pe12.5)') '  ss = ',ss(nk+1)
!!! write(6,'(a,1pe12.5)') ' err = ',errgmres
!!! call flush(6)
!!! endif

  ! ym minimizes ||beta*e1-hbarre.y||_2

  nit     = nit + 1

enddo

!!! if ( debug_flag == 1 ) then
!!! write(6,'(a,1pe12.5)') ' ref = ',ref*def_impli%maxres
!!! write(6,'(a,1pe12.5)') ' err = ',errgmres
!!! call flush(6)
!!! endif

if (nit <= def_impli%max_it) then
  info = nit - 1
else
  info = -1
endif

deallocate(w1, v1)
deallocate(r1, p1)
deallocate(hh)
deallocate(qc, qs, ss, yy)

endsubroutine bdlu_gmres

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2008 : creation
!------------------------------------------------------------------------------!

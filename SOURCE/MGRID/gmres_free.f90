!------------------------------------------------------------------------------!
! Procedure : gmres_free                        Authors : G. Grondin
!
! Function
!   Resolution of linear system : mat.sol = rhs
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine gmres_free(dt, info, defsolver, gridlist, coupling, ncoupling, mat)

use MATFREE
use SPARSE_MAT

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling       ! number of couplings of the zone
type(st_spmat)         :: mat

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! coupling data

! -- Inputs/Outputs --

! -- Outputs --

! -- Internal variables --
real(krp), dimension(:),   allocatable :: qc, qs, ss, yy
real(krp), dimension(:,:), allocatable :: hh
type(st_genericfield), dimension(:),   pointer :: r1, p1
type(st_genericfield), dimension(:,:), pointer :: w1, v1
type(st_genericfield), dimension(:),   pointer :: uref, rref
type(st_genericfield), dimension(:),   pointer :: sol
type(st_grid), pointer               :: pgrid
type(mnu_imp) :: def_impli
type(st_fieldinfo) :: field_info
integer(kip)  :: infogmres

integer(kip)                         :: nit
real(krp)                            :: errgmres, ref
real(krp)                            :: beta, fact, tmp, normp1

integer(kip)                         :: ik, jk, nk
integer(kip)                         :: ngrid, igrid
integer(kip)                         :: nkrylov
integer(kip)                         :: i, ib, is
integer(kip)                         :: ip, ic1
real(krp), allocatable :: tabres(:)
character*256 :: message
real(krp), dimension(64,64) :: matrix

! -- Body --

nkrylov = def_impli%nkrylov

ngrid  = gridlist%nbgrid
field_info%dt        = dt
field_info%info      = info
field_info%defsolver = defsolver
field_info%gridlist  = gridlist
field_info%ncoupling = ncoupling
def_impli = defsolver%deftime%implicite

! initialisation

nit    = 0
errgmres = huge(errgmres)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate(qc(nkrylov))
allocate(qs(nkrylov))
allocate(ss(nkrylov+1))
allocate(yy(nkrylov))
allocate(hh(nkrylov+1,nkrylov))

allocate(r1(ngrid))
allocate(p1(ngrid))

allocate(w1(nkrylov,ngrid))
allocate(v1(nkrylov,ngrid))

allocate(uref(ngrid))
allocate(sol(ngrid))
! allocations
pgrid => gridlist%first
do igrid = 1, ngrid
  call new_genfield(r1(igrid), pgrid%field%etatcons)
  call new_genfield(p1(igrid), pgrid%field%etatcons)
  do jk = 1, nkrylov
    call new_genfield(w1(jk,igrid), pgrid%field%etatcons)
    call new_genfield(v1(jk,igrid), pgrid%field%etatcons)
  enddo
  call new_genfield(uref(igrid), pgrid%field%etatcons)
  call new_genfield(sol(igrid), pgrid%field%etatcons)
  pgrid => pgrid%next
enddo
rref => p1

! back-up etatcons
pgrid => gridlist%first
do igrid = 1, ngrid
  call transfer(uref(igrid), pgrid%field%etatcons)
  call transfer(rref(igrid), pgrid%field%residu)
  pgrid => pgrid%next
enddo


! -- initialization --

! p1 = rref = residu

! -- initial guess --

pgrid => gridlist%first
do igrid = 1, ngrid
  call transfer(sol(igrid), p1(igrid))
  do ic1 = 1, pgrid%umesh%ncell_int
    do ip = 1, sol(igrid)%nscal
      sol(igrid)%tabscal(ip)%scal(ic1) = 1._krp & !(pgrid%dtloc(ic1)/pgrid%umesh%mesh%volume(ic1,1,1)) &
                                         * sol(igrid)%tabscal(ip)%scal(ic1)
    enddo
    do ip = 1, sol(igrid)%nvect
      sol(igrid)%tabvect(ip)%vect(ic1) = 1._krp & !(pgrid%dtloc(ic1)/pgrid%umesh%mesh%volume(ic1,1,1)) &
                                         * sol(igrid)%tabvect(ip)%vect(ic1)  
    enddo
  enddo
!!  allocate(tabres(size_tot(sol(igrid))))
!!  call packst(sol(igrid), tabres, size_tot(sol(igrid)))
!!  do i = 1, mat%dlu%dim
!!    tabres(i) = tabres(i) / mat%dlu%diag(i)  ! initial guess
!!  enddo
!!  do i = 1, mat%bdlu%dim
!!    is = (i-1)*mat%bdlu%dimblock
!!    do ib = 1, mat%bdlu%dimblock
!!      tabres(is+ib) = tabres(is+ib) / mat%bdlu%diag(ib,ib,i)  ! initial guess
!!    enddo
!!  enddo
!!  call unpackst(tabres, sol(igrid), size_tot(sol(igrid)))
!!  deallocate(tabres)
  pgrid => pgrid%next
enddo

call yeqax_free(w1(1,1:ngrid), sol, &
                uref, rref, field_info)                         ! wj = M^(-1).A.vj
ref = dot_prod(w1(1,1:ngrid),p1)/l2sqnorm(w1(1,1:ngrid))
!call scale(sol, ref)

call xeqxpay(w1(1,1:ngrid), -1.0_krp, p1)
call scale(sol,1.0E-02_krp)
call yeqax_free(w1(1,1:ngrid), sol, &
                uref, rref, field_info)                         ! wj = M^(-1).A.vj
call scale(sol,1.0E+02_krp)
call scale(w1(1,1:ngrid),1.0E+02_krp)
call xeqxpay(w1(1,1:ngrid), -1.0_krp, p1)

! ref = norme_L1(sol)
ref = l2norm(sol)

! normp1 = norme_L2(p1)
normp1 = l2norm(p1)

do while ((errgmres >= ref*def_impli%maxres).and.(nit <= def_impli%max_it*2))

  ! r0 = M^(-1) . ( b - A.x0 )
! Verification A.X
if (1==2) then
  do is = 1,500
    ! Annulation v1
    call transfer(v1(1,1:ngrid), sol)
    call scale(v1(1,1:ngrid), 0._krp)
    ! Composante is
    if ( mod(is-1,5) <= 1 ) then
      v1(1,1)%tabscal(mod(is,5))%scal((is+4)/5) = &
      sol(1)%tabscal(mod(is,5))%scal((is+4)/5)
    else
      select case(mod(is-2,5))
      case( 1 )
      v1(1,1)%tabvect(1)%vect((is+4)/5)%x = &
      sol(1)%tabvect(1)%vect((is+4)/5)%x
      case( 2 )
      v1(1,1)%tabvect(1)%vect((is+4)/5)%y = &
      sol(1)%tabvect(1)%vect((is+4)/5)%y
      case( 3 )
      v1(1,1)%tabvect(1)%vect((is+4)/5)%z = &
      sol(1)%tabvect(1)%vect((is+4)/5)%z
      end select
    endif
    ! Affichage AAsol
    call yeqax_free(r1, v1(1,1:ngrid), uref, rref, field_info)       ! R1 = RHS - MAT.SOL
  enddo
  stop
endif
!  call scale(sol(1:ngrid), 0._krp)
!  sol(1)%tabscal(2)%scal(1) = 2._krp
!  call yeqax_free(r1, sol, uref, rref, field_info)       ! R1 = RHS - MAT.SOL
!
!  beta = l2norm(r1)                                             ! beta = ||r0||_2
!stop
  call yeqmaxpz_free(r1, sol, p1, uref, rref, field_info)       ! R1 = RHS - MAT.SOL

  beta = l2norm(r1)                                             ! beta = ||r0||_2
  fact = 1.0_krp/beta

  call transfer(v1(1,1:ngrid), r1)                     ! v1 = r0/|r0|
  call scale(v1(1,1:ngrid), fact)

  ss(1:nkrylov+1) = 0.0_krp
  ss(1) = beta                                                  ! s(1) = |r0|

  hh(1:nkrylov+1,1:nkrylov) = 0.0_krp

  do jk = 1,nkrylov

    call yeqax_free (w1(jk,1:ngrid), v1(jk,1:ngrid), &
                     uref, rref, field_info)                    ! wj = M^(-1).A.vj

    do ik = 1,jk
      hh(ik,jk) = dot_prod(w1(jk,1:ngrid),v1(ik,1:ngrid))       ! hij = (wj,vi)
      call xeqxpay(w1(jk,1:ngrid),-hh(ik,jk),v1(ik,1:ngrid))    ! wj = wj - hij*vi
    enddo

    hh(jk+1,jk) = l2norm(w1(jk,1:ngrid))                        ! h(j+1,j) = ||wj||_2

    do ik = 1,jk-1                                            ! apply J(1)...J(kj-1) on hh
      tmp = hh(ik,jk)
      hh(ik  ,jk) =  qc(ik)*tmp + qs(ik)*hh(ik+1,jk)
      hh(ik+1,jk) = -qs(ik)*tmp + qc(ik)*hh(ik+1,jk)
    enddo

    if ( hh(jk+1,jk) > epsilon(normp1)*10*normp1 ) then

      if ( jk < nkrylov ) then
        fact = 1.0_krp/hh(jk+1,jk)
        call transfer(v1(jk+1,1:ngrid), w1(jk,1:ngrid))! v(j+1) = wj/h(j+1,j)
        call scale(v1(jk+1,1:ngrid), fact)
      endif

!      do ik = 1,jk-1                                            ! apply J(1)...J(kj-1) on hh
!        tmp = hh(ik,jk)
!        hh(ik  ,jk) =  qc(ik)*tmp + qs(ik)*hh(ik+1,jk)
!        hh(ik+1,jk) = -qs(ik)*tmp + qc(ik)*hh(ik+1,jk)
!      enddo

      if ( hh(jk,jk) == 0 ) exit                                ! Exit if hh(jk,jk)==0

      tmp = sqrt(hh(jk,jk)**2+hh(jk+1,jk)**2)                   ! build J(jk)
      qc(jk) = hh(jk  ,jk)/tmp
      qs(jk) = hh(jk+1,jk)/tmp

      hh(jk  ,jk) = tmp
      hh(jk+1,jk) = 0.0_krp

      ss(jk+1) = -qs(jk)*ss(jk)                                 ! apply J(jk) on ss
      ss(jk  ) =  qc(jk)*ss(jk)

      if ( abs(ss(jk+1)) < def_impli%maxres*abs(ss(1)) ) exit

    else

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
    call xeqxpay(sol, yy(jk), v1(jk,1:ngrid))                   ! sol = sol + Vm.ym
  enddo

  call yeqmaxpz_free(r1, sol, p1, uref, rref, field_info)       ! R1 = RHS - MAT.SOL
  ss(nk+1) =  l2norm(r1)

  errgmres = ss(nk+1)/normp1

  nit     = nit + 1

enddo

if (nit <= def_impli%max_it) then
  infogmres = nit - 1
else
  infogmres = -1
endif

! back-up etatcons
pgrid => gridlist%first
do igrid = 1, ngrid
  call transfer(pgrid%field%etatcons, uref(igrid))
  call transfer(pgrid%field%residu, sol(igrid))
  pgrid => pgrid%next
enddo


do igrid = 1, ngrid
  call delete(r1(igrid))
  call delete(p1(igrid))
  do jk = 1, nkrylov
    call delete(w1(jk,igrid))
    call delete(v1(jk,igrid))
  enddo
  call delete(uref(igrid))
  call delete(sol(igrid))
enddo
deallocate(uref, sol)
deallocate(w1, v1)
deallocate(r1, p1)
deallocate(hh)
deallocate(qc, qs, ss, yy)

endsubroutine gmres_free


!------------------------------------------------------------------------------!
! Changes history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!

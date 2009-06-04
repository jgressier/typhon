!------------------------------------------------------------------------------!
! Procedure : dlu_jacobi                          Authors : J. Gressier
!                                                 Created : Avril 2004
! Fonction
!   Resolution of linear system : mat.sol = rhs
!     mat type(st_dlu)
!     iterative method JACOBI : mat = D + L + U
!       sol(n+1) = D^(-1).-(L+U).sol(n) + D^(-1).rhs
!
! Defauts/Limitations/Divers :
!   - Array sol(*) contains rhs as input
!
!------------------------------------------------------------------------------!
subroutine dlu_jacobi(def_impli, mat, sol, info)

use TYPHMAKE
use SPARSE_MAT
use LAPACK
use MENU_NUM

implicit none

! -- Inputs --
type(mnu_imp) :: def_impli
type(st_dlu)  :: mat

! -- Inputs/Outputs --
real(krp)     :: sol(1:mat%dim)  ! RHS as input, SOLUTION as output

! -- Outputs --
integer(kip)  :: info

! -- Internal variables --
real(krp), dimension(:), allocatable :: vec, p1
integer(kip)                         :: nit, dim, if, imin, imax
real(krp)                            :: err, ref

! -- Body --

dim = mat%dim

! initialisation

nit    = 0
err = huge(err)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate( vec(dim))
allocate(p1(dim))

! -- initialization --

p1 (1:dim) = sol(1:dim)  ! save RHS
sol(1:dim) = p1(1:dim) / mat%diag(1:dim)  ! initial guess
ref = sum(abs(sol(1:dim)))

!call sort_dlu(mat)

do while ((err >= ref*def_impli%maxres).and.(nit <= def_impli%max_it))

  vec(1:dim) = p1(1:dim)

  ! multiplication par (L + U)

  do if = 1, mat%ncouple
    imin = mat%couple%fils(if,1) ! ic1 cell is supposed to be the lowest index
    imax = mat%couple%fils(if,2) ! ic2 cell is supposed to be the highest index
    if (imax <= dim) then
      vec(imax) = vec(imax) - mat%lower(if)*sol(imin)
      vec(imin) = vec(imin) - mat%upper(if)*sol(imax)
    endif
  enddo

  ! division par D (coef par coef)

  vec(1:dim) = vec(1:dim) / mat%diag(1:dim)

  ! error computation & update
  err  = sum(abs(sol(1:dim)-vec(1:dim)))
  !print*,'conv jacobi',nit,log10(err/ref)
  sol(1:dim) = vec(1:dim)

  nit     = nit + 1

enddo

if (nit <= def_impli%max_it) then
  info = nit - 1
else
  info = -1
endif

deallocate(vec, p1)

endsubroutine dlu_jacobi

!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2004 : creation
!------------------------------------------------------------------------------!

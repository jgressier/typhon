!------------------------------------------------------------------------------!
! Procedure : bdlu_bicg                              Authors : J. Gressier
!                                                    Created : August 2005
! Fonction
!   Resolution of linear system : mat.sol = rhs
!     mat type(st_bdlu)
!     non stationary iterative method BICG
!
! Defauts/Limitations/Divers :
!   - Array sol(*) contains rhs as input
!
!------------------------------------------------------------------------------!
subroutine bdlu_bicg(def_impli, mat, sol, info)

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
real(krp), dimension(:), allocatable :: r1, r2, p1, p2, q1, q2
integer(kip)                         :: nit, i, ib, is, dim
real(krp)                            :: erreur, ref
real(krp)                            :: rho0, rho1, beta, alpha

! -- Body --

dim = mat%dim*mat%dimblock

! initialisation

nit    = 0
erreur = huge(erreur)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate(r1(dim)) ;     allocate(r2(dim))
allocate(p1(dim)) ;     allocate(p2(dim))
allocate(q1(dim)) ;     allocate(q2(dim))

! -- initialization --

p1 (1:dim) = sol(1:dim)  ! save RHS

! -- initial guess --

do i = 1, mat%dim
  is = (i-1)*mat%dimblock
  do ib = 1, mat%dimblock
    sol(is+ib) = p1(is+ib) / mat%diag(ib,ib,i)  ! initial guess
  enddo
enddo
ref = sum(abs(sol(1:dim)))

!call sort_bdlu(mat)

call bdlu_yeqmaxpz(r1(1:dim), mat, sol(1:dim), p1(1:dim))  ! R1 = RHS - MAT.SOL
r2(1:dim) = r1(1:dim)                                      ! R2 = R1

do while ((erreur >= ref*def_impli%maxres).and.(nit <= def_impli%max_it))

  ! Precond : rho1 = dotprod( M(-1).r1 , r2 )

  rho1 = dot_product(r1(1:dim), r2(1:dim))

  ! Precond : p1 = M(-1).r1 [ + beta*p1 ]
  !           p2 = M(-1)transp.r2 [ + beta_conj*p2 ]

  if (nit == 0) then
    p1(1:dim) = r1(1:dim)
    p2(1:dim) = r2(1:dim)
  else
    beta = rho1 / rho0
    p1(1:dim) = r1(1:dim) + beta*p1(1:dim)
    p2(1:dim) = r2(1:dim) + beta*p2(1:dim)
  endif

  call bdlu_yeqax (q1(1:dim), mat, p1(1:dim))
  call bdlu_yeqatx(q2(1:dim), mat, p2(1:dim))

  alpha = rho1 / dot_product(p2(1:dim), q1(1:dim))

  ! error computation & update
  erreur  = abs(alpha)*sum(abs(p1(1:dim)))
  sol(1:dim) = sol(1:dim) + alpha*p1(1:dim)
  !print*,'conv bicg',nit,log10(erreur/ref), rho1

  ! Precond : r2 = r2 - alfa_conj*q2

  ! prepare next iteration
  r1(1:dim) = r1(1:dim) - alpha*q1(1:dim)
  r2(1:dim) = r2(1:dim) - alpha*q2(1:dim)
  rho0 = rho1

  nit     = nit + 1

enddo

if (nit <= def_impli%max_it) then
  info = nit - 1
else
  info = -1
endif

deallocate(r1, r2, p1, p2, q1, q2)

endsubroutine bdlu_bicg

!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!

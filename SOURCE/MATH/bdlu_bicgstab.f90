!------------------------------------------------------------------------------!
! Procedure : bdlu_bicgstab                          Authors : J. Gressier
!                                                    Created : August 2005
! Fonction
!   Resolution of linear system : mat.sol = rhs
!     mat type(st_bdlu)
!     non stationary iterative method BICGSTAB
!
! Defauts/Limitations/Divers :
!   - Array sol(*) contains rhs as input
!
!------------------------------------------------------------------------------!
subroutine bdlu_bicgstab(def_impli, mat, sol, info)

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
real(krp), dimension(:), allocatable :: r1, r2, p, s, t, v
integer(kip)                         :: nit, i, ib, is, dim
real(krp)                            :: erreur, ref
real(krp)                            :: rho0, rho1, beta, alpha, omega

! -- Body --

dim = mat%dim*mat%dimblock

! initialisation

nit    = 0
erreur = huge(erreur)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate(r1(dim)) ;     allocate(r2(dim))
allocate(p (dim)) ;     allocate(s (dim))
allocate(t (dim)) ;     allocate(v (dim))

! -- initialization --

p(1:dim) = sol(1:dim)  ! save RHS

! -- initial guess --

do i = 1, mat%dim
  is = (i-1)*mat%dimblock
  do ib = 1, mat%dimblock
    sol(is+ib) = p(is+ib) / mat%diag(ib,ib,i)  ! initial guess
  enddo
enddo
ref = sum(abs(sol(1:dim)))

!call sort_bdlu(mat)

call bdlu_yeqmaxpz(r1(1:dim), mat, sol(1:dim), p(1:dim))   ! R1 = RHS - MAT.SOL
r2(1:dim) = r1(1:dim)                                      ! R2 = R1

do while ((erreur >= ref*def_impli%maxres).and.(nit <= def_impli%max_it))

  rho1 = dot_product(r1(1:dim), r2(1:dim))

  if (nit == 0) then
    p(1:dim) = r1(1:dim)
  else
    beta = (rho1 / rho0)*(alpha/omega)
    p(1:dim) = p (1:dim) - omega* v(1:dim)
    p(1:dim) = r1(1:dim) + beta * p(1:dim)
  endif

  ! no preconditioning

  call bdlu_yeqax (v(1:dim), mat, p(1:dim))

  alpha = rho1 / dot_product(r2(1:dim), v(1:dim))
  s(1:dim) = r1(1:dim) - alpha*v(1:dim)

  ! no preconditioning

  call bdlu_yeqax (t(1:dim), mat, s(1:dim))
  omega = dot_product(t(1:dim), s(1:dim)) / sum(t(1:dim)**2)

  ! error computation & update
  erreur  = sum(abs(alpha*p(1:dim) + omega*s(1:dim)))
  sol(1:dim) = sol(1:dim) + alpha*p(1:dim) + omega*s(1:dim)
  !print*,'conv bicgstab',nit,log10(erreur/ref), rho1

  ! prepare next iteration
  r1(1:dim) = s(1:dim) - omega*t(1:dim)
  !print*,'   r',abs(sum(r1(1:dim))), '   s',abs(sum(s(1:dim)))
  rho0 = rho1

  nit     = nit + 1

enddo

if (nit <= def_impli%max_it) then
  info = nit - 1
else
  info = -1
endif

deallocate(r1, r2, p, s, t, v)

endsubroutine bdlu_bicgstab

!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!

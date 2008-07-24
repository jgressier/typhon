!------------------------------------------------------------------------------!
! Procedure : implicit_solve                            Authors : J. Gressier
!                                                       Created : August 2005
! Fonction
!   Solve implicit system
!
!------------------------------------------------------------------------------!
subroutine implicit_solve(deftime, mat, rhsfield)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use GENFIELD
use MATRIX_ARRAY
use SPARSE_MAT

implicit none

! -- Inputs --
type(mnu_time)         :: deftime       ! time integration parameter
type(st_spmat)         :: mat

! -- Input/output --
type(st_genericfield)  :: rhsfield      ! rhs generic field

! -- Internal variables --
real(krp), allocatable :: tabres(:)     ! collect residuals
integer(kip)           :: if, ic1, ic2, ic, info, dim

! -- BODY --

!--------------------------------------------------
! solve implicit system
!--------------------------------------------------

allocate(tabres(size_tot(rhsfield)))

call packst(rhsfield, tabres, size_tot(rhsfield))

! CALL SOLVE IMPLICIT SYSTEM
! resolution

select case(deftime%implicite%methode)
case(alg_lu)
  call dlu_lu(mat%dlu, tabres)

case(alg_jac)
  call solve_jacobi(deftime, mat, tabres, info)
  if (info < 0) call print_warning("JACOBI iterative method not converged")

case(alg_gs)
  call erreur("development","Gauss-Seidel method not implemented")

case(alg_sor)
  call erreur("development","SOR method not implemented")

case(alg_bicg)
  call solve_bicg(deftime, mat, tabres, info)
  if (info < 0) call print_warning("BICG iterative method not converged")

case(alg_bicgpjac)
  call solve_bicg_pjacobi(deftime, mat, tabres, info)
  if (info < 0) call print_warning("BICG-Jacobi iterative method not converged")

case(alg_cgs)
  call solve_cgs(deftime, mat, tabres, info)
  if (info < 0) call print_warning("CGS iterative method not converged")

case(alg_bicgstab)
  call solve_bicgstab(deftime, mat, tabres, info)
  if (info < 0) call print_warning("BICG-Stabilized iterative method not converged")

case(alg_gmres)
  call solve_gmres(deftime, mat, tabres, info)
  if (info < 0) call print_warning("GMRES iterative method not converged")

case default
  call erreur("Internal error","Unknown algebraic inversion method")
endselect

call unpackst(tabres, rhsfield, size_tot(rhsfield))

deallocate(tabres)


endsubroutine implicit_solve
!------------------------------------------------------------------------------!
! Change History
!
! Aug  2005 : creation (from part of implicit_step)
! Dec  2006 : add GMRES method
!------------------------------------------------------------------------------!

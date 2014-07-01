!------------------------------------------------------------------------------!
! Procedure : calc_varprim                      Authors : J. Gressier
!
! Fonction
!   Compute primitive variables from conservative variables
!   Redirect to solver-specific subroutines
!
!------------------------------------------------------------------------------!
subroutine calc_varprim(defsolver, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD

implicit none

! -- Inputs --
type(mnu_solver) :: defsolver       ! solver parameters

! -- Inputs/Outputs --
type(st_field)   :: field            ! prim/cons field

! -- Internal variables --

! -- Body --

select case(defsolver%typ_solver)
case(solKDIF)
  call calc_varprim_kdif(defsolver%defkdif, field)
case(solNS)
  call calc_varprim_ns(defsolver%defns, field, defsolver%nsim)
case(solVORTEX)
  ! nothing to do
case default
  call error_stop("Internal error (calc_varprim): unknown solver type: "// &
              trim(strof(defsolver%typ_solver)))
endselect

!-----------------------------
endsubroutine calc_varprim
!------------------------------------------------------------------------------!
! History
!
! Jun 2003: creation
! Jul 2004: NS solver (calc_varprim_ns)
!------------------------------------------------------------------------------!

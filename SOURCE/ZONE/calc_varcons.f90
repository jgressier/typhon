!------------------------------------------------------------------------------!
! Procedure : calc_varcons                      Authors : J. Gressier
!
! Fonction
!   Compute conservative variables from primitive variables
!   Redirect to solver-specific subroutines
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_varcons(def_solver, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD

implicit none

! -- Inputs --
type(mnu_solver) :: def_solver       ! solver parameters

! -- Inputs/Outputs --
type(st_field)   :: field            ! prim/cons field

! -- Internal variables --

! -- Body --

select case(def_solver%typ_solver)
case(solKDIF)
  call calc_varcons_kdif(def_solver%defkdif, field)
case(solNS)
  call calc_varcons_ns(def_solver%defns, field)
case default
  call error_stop("Internal error (calc_varcons): unknown solver type: "// &
              trim(strof(def_solver%typ_solver)))
endselect


!-----------------------------
endsubroutine calc_varcons

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Jun 2003: creation
! Jul 2004: NS solver (calc_varcons_ns)
!------------------------------------------------------------------------------!

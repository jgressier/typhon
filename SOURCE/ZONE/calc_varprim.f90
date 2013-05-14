!------------------------------------------------------------------------------!
! Procedure : calc_varprim                      Authors : J. Gressier
!
! Fonction
!   Compute primitive variables from conservative variables
!   Redirect to solver-specific subroutines
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_varprim(def_solver, field)

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
  call calc_varprim_kdif(def_solver%defkdif, field)
case(solNS)
  call calc_varprim_ns(def_solver%defns, field)
case(solVORTEX)
  ! nothing to do
case default
  call error_stop("Internal error (calc_varprim): unknown solver type: "// &
              trim(strof(def_solver%typ_solver)))
endselect


!-----------------------------
endsubroutine calc_varprim

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Jun 2003: creation
! Jul 2004: NS solver (calc_varprim_ns)
!------------------------------------------------------------------------------!

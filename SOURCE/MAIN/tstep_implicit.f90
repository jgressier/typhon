!------------------------------------------------------------------------------!
! Procedure : tstep_implicit                    Authors : J. Gressier
!
! Function
!   Implicit integration of the domain
!
!------------------------------------------------------------------------------!
subroutine tstep_implicit(dtloc, typtemps, defsolver, &
                          umesh, field, coupling, ncoupling, mat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MATRIX_ARRAY
use SPARSE_MAT
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
character        :: typtemps         ! time model (STEADY, UNSTEADY, PERIODIC)
type(mnu_solver) :: defsolver        ! solver parameters
type(st_ustmesh) :: umesh            ! unstructured mesh
real(krp)        :: dtloc(1:umesh%ncell)         ! CFL time step
integer          :: ncoupling        ! number of couplings of the zone

! -- Inputs/Outputs --
type(st_field)   :: field            ! field
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling         ! coupling data
type(st_spmat)   :: mat

! -- Internal variables --

! -- Body --

!--------------------------------------------------
! solve implicit system
!--------------------------------------------------

call implicit_solve(defsolver%deftime, mat, field%residu)

call delete(mat)


endsubroutine tstep_implicit
!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2004 : creation
! Aug 2005 : split / call build_implicit to handle different structures
!            split / call implicit_solve
! Sep 2005 : local time stepping
! Nov 2007 : new name "tstep_implicit"
!------------------------------------------------------------------------------!

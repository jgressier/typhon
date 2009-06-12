!------------------------------------------------------------------------------!
! Procedure : init_implicit                             Authors : G. Grondin
!                                                       Created : Jun 2009
! Function
!   Init implicit matrix
!
!------------------------------------------------------------------------------!
subroutine init_implicit(dtloc, defsolver, umesh, mat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use SPARSE_MAT

implicit none

! -- Inputs --
type(st_ustmesh) :: umesh        ! unstructured mesh
real(krp)        :: dtloc(1:umesh%ncell)
type(mnu_solver) :: defsolver    ! solver parameters

! -- Inputs/Outputs --

! -- Outputs --
type(st_spmat)   :: mat

! -- Internal variables --

! -- Body --

!-----------------------------------------------------
! init SPARSE MATRIX
!-----------------------------------------------------

mat%type = defsolver%deftime%implicite%storage

select case(mat%type)

case(mat_dlu)
  call init_implicit_dlu(dtloc, defsolver, umesh, mat%dlu)

case(mat_bdlu)
  call init_implicit_bdlu(dtloc, defsolver, umesh, mat%bdlu)

case(mat_crs, mat_bcrs)
  call erreur("development","unexpected matrix structure (not yet implemented)")

case default
  call erreur("internal error","unknown matrix structure to init implicit system")

endselect


endsubroutine init_implicit
!------------------------------------------------------------------------------!
! Change history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!

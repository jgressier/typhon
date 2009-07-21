!------------------------------------------------------------------------------!
! Procedure : outputzone_ustmesh
!                         
! Function 
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_ustmesh(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer                :: dim, ufc, ir, izone
integer                :: info
type(st_grid), pointer :: pgrid
type(st_genericfield)  :: vfield
character(len=10)      :: suffix
integer                :: isize(3)     ! info array for zone

! -- BODY --

pgrid => zone%gridlist%first

select case(defio%format)

case(fmt_TECPLOT)
  call error_stop("'Internal error) Unable to use general output with TECPLOT format")

case(fmt_VTK, fmt_VTKBIN)

  call writevtk_ustmesh(defio, pgrid%umesh)

case(fmt_CGNS, fmt_CGNS_linked)

  isize(1) = pgrid%umesh%nvtex       ! vertex size 
  isize(2) = pgrid%umesh%ncell_int   ! cell size
  isize(3) = 0                       ! boundary vertex size (zero if vertices not sorted)

  ! -- create CGNS zone --

  call cg_zone_write_f(defio%iunit, defio%izone, "ustmesh", isize, Unstructured, izone, info)

  ! -- write TYPHON ustmesh to CGNS zone (defio%izone is a CGNS base index) --

  call writecgns_ustmesh(defio%iunit, defio%izone, izone, pgrid%umesh)

  call writecgns_bocomesh(defio%iunit, defio%izone, izone, pgrid%umesh)

case default
  call error_stop("Internal error (outputzone_ustmesh): unknown output format parameter")
endselect

endsubroutine outputzone_ustmesh
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!

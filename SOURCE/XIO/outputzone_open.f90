!------------------------------------------------------------------------------!
! Procedure : outputzone_open
!
! Function
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_open(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN
use TYPHON_FMT

implicit none

#ifdef CGNS
include 'cgnslib_f.h'
#endif/*CGNS*/

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, ir
integer               :: ibase          ! CGNS base index
integer               :: info, nbmesh, nbsol
type(st_genericfield) :: vfield
character(len=8)      :: suffix

! -- BODY --

select case(defio%format)

case(fmt_TECPLOT)
  call error_stop("Internal error (outputzone_open): Unable to use general output with TECPLOT format")

case(fmt_TYPHON)

  suffix = "."//xtyext_sol
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  nbmesh = 1
  nbsol  = 1
  call typhon_openwrite(defio%iunit, trim(defio%filename)//trim(suffix), defio%deftyphon, nbmesh, nbsol, defio%meshdef)

case(fmt_VTK)

  suffix = ".vtk"
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  call vtkasc_openwrite(defio%iunit, trim(defio%filename)//trim(suffix), defio%defvtk)

case(fmt_VTKBIN)

  suffix = ".vtk"
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  call vtkbin_openwrite(defio%iunit, trim(defio%filename)//trim(suffix), defio%defvtk)

case(fmt_CGNS, fmt_CGNS_linked)

#ifdef CGNS
  suffix = '.cgns'
  call cg_open_f(trim(defio%filename)//trim(suffix), MODE_WRITE, ufc, info)
  if (info /= 0) &
    call error_stop("CGNS IO error: opening CGNS file (error "//trim(strof(info))//")")
  defio%iunit = ufc

  call cg_base_write_f(defio%iunit, "Z_"//zone%name, geodim(zone%gridlist%first%umesh), 3, ibase, info)
  if (info /= 0) &
    call error_stop("CGNS IO error: writing CGNS base (error "//trim(strof(info))//")")
  defio%izone = ibase
#else /*CGNS*/
  call error_stop("Internal error (outputzone_open): CGNS format was not activated at configure time")
#endif/*CGNS*/

case default
  call error_stop("Internal error (outputzone_open): unknown output format parameter")
endselect

endsubroutine outputzone_open
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!

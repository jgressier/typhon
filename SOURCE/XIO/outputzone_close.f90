!------------------------------------------------------------------------------!
! Procedure : outputzone_close
!
! Function
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_close(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

#ifdef CGNS
include 'cgnslib_f.h'
#endif/*CGNS*/

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: info

! -- BODY --

select case(defio%format)

case(fmt_TECPLOT)
  call error_stop("Internal error (outputzone_close): Unable to use general output with TECPLOT format")
case(fmt_VTK, fmt_VTKBIN)
  call close_io_unit(defio%iunit)
case(fmt_TYPHON)
  call typhon_close(defio%deftyphon)
#ifdef CGNS
case(fmt_CGNS, fmt_CGNS_linked)
  call cg_close_f(defio%iunit, info)
  defio%iunit = 0
#endif/*CGNS*/
case default
  call error_stop("Internal error (outputzone_close): unknown output format parameter")
endselect

endsubroutine outputzone_close
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!

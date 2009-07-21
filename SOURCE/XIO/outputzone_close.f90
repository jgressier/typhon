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

include 'cgnslib_f.h'

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: info

! -- BODY --

select case(defio%format)

case(fmt_TECPLOT)
  call error_stop("'Internal error) Unable to use general output with TECPLOT format")
case(fmt_VTK, fmt_VTKBIN)
  call close_io_unit(defio%iunit)
case(fmt_CGNS, fmt_CGNS_linked)
  call cg_close_f(defio%iunit, info)
  defio%iunit = 0
case default
  call error_stop("Internal error (outputzone_close): unknown output format parameter")
endselect

endsubroutine outputzone_close
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!

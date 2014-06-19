!------------------------------------------------------------------------------!
! Procedure : outputzone_sol
!
! Function
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_sol(defio, zone, isim)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN
use TYFMT_SOL
use VTKSOL

implicit none

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone
integer 	      :: isim 	   ! number of current simulation
! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, izone, nsim
integer               :: info
type(st_genericfield) :: vfield
type(st_grid), pointer :: pgrid

! -- BODY --
nsim = zone%defsolver%nsim

pgrid => zone%gridlist%first
select case(defio%format)

case(fmt_TYPHON)
  call typhonwrite_sol(defio%deftyphon, pgrid%umesh, pgrid%info%field_loc%etatprim)

case(fmt_TECPLOT)
  call error_stop("(Internal error) Unable to use general output with TECPLOT format")

case(fmt_VTK, fmt_VTKBIN)

  call writevtk_sol(defio%defvtk, pgrid%umesh, pgrid%info%field_loc%etatprim, isim, nsim)
#ifdef CGNS
case(fmt_CGNS, fmt_CGNS_linked)

  izone = 1
  call writecgns_sol(defio%iunit, defio%izone, izone, &
                     pgrid%umesh, pgrid%info%field_loc%etatprim)
#endif/*CGNS*/

case default
  call error_stop("Internal error (outputzone_sol): unknown output format parameter")
endselect

endsubroutine outputzone_sol
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!

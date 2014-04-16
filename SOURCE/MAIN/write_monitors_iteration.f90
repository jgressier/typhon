!------------------------------------------------------------------------------!
! Procedure : write_monitors_iteration
!             
! Fonction    
!   Calcul des quantites definies par les write_monitors
!
!------------------------------------------------------------------------------!
subroutine write_monitors_iteration(zinfo, force)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODINFO
use MENU_GEN

implicit none

! -- Inputs --
type(st_infozone), intent(inout) :: zinfo      ! zone information (residuals)
logical                          :: force

! -- Outputs --

! -- Internal variables --
integer, parameter :: nlines = 25

! -- BODY --

!-----------------------------------------------------
! write file monitor

select case(zinfo%time_model)
case(time_steady)
  write(uf_monres,*) zinfo%iter_tot, log10(zinfo%cur_res)
endselect

!-----------------------------------------------------
! print screen monitor

! -- header --
if (mod(zinfo%iter_loc, zinfo%itfreq_screen*nlines) == 1) then 
  call moninfo_header(str_w, zinfo)
  call print_master(9,str_w)
endif

if ((mod(zinfo%iter_loc,zinfo%itfreq_screen) == 0).or.force) then
  call moninfo_monitor(str_w, zinfo)
  call print_master(9,str_w)
endif

!-----------------------------
endsubroutine write_monitors_iteration

!------------------------------------------------------------------------------!
! Changes history
!
! mai 2003: creation de la procedure (test pour debuggage)
! nov 2003: redirection selon type de capteur
! Apr 2008: change name into write_monitors.f90
! Jun 2009: change name into write_monitors_iteration.f90
! Apr 2014: transfer screen printing from check_end_cycle
!------------------------------------------------------------------------------!

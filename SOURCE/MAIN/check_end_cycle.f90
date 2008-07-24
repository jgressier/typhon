!------------------------------------------------------------------------------!
! Procedure : end_cycle                           Author  : J. Gressier
!                                                 Created : May 2006
! Fonction 
!   Write cycle information (standard output)
!   and check end cycle
!
!------------------------------------------------------------------------------!
subroutine check_end_cycle(zinfo, dt)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODINFO
use MENU_GEN

implicit none

! -- INPUTS --
real(krp), intent(in)        :: dt         ! time step of current iteration

! -- INPUTS/OUTPUTS --
type(st_infozone), intent(inout) :: zinfo      ! zone information (residuals)

! -- Internal variables --
integer, parameter :: itfreq = 10
integer, parameter :: nlines = 25
integer :: ierr

! -- BODY --

select case(zinfo%time_model)

case(time_steady)
  zinfo%residu_ref = max(zinfo%residu_ref, zinfo%cur_res)
  if (zinfo%cur_res/zinfo%residu_ref <= zinfo%residumax) zinfo%end_cycle = .true.
  ! -- header --
  if (mod(zinfo%iter_loc, itfreq*nlines) == 1) call print_info(9,'     it residuals')
  ! -- residuals --
  if (mod(zinfo%iter_loc,itfreq) == 0) &
      write(str_w,'(i7,g12.4)') zinfo%iter_loc, log10(zinfo%cur_res) !   log10(zinfo%cur_res/zinfo%residu_ref)
      !if (mod(zinfo%iter_loc,itfreq) == 0) call print_info(9,str_w)

case(time_unsteady, time_unsteady_inverse)
  zinfo%cycle_time = zinfo%cycle_time + dt
  ! -- header --
  if (mod(zinfo%iter_loc, itfreq*nlines) == 1) call print_info(9,'     it time')
  ! -- residuals --
  if (mod(zinfo%iter_loc,itfreq) == 0) &      !    if (zinfo%end_cycle) &
      write(str_w,'(i7,g11.4)') zinfo%iter_loc, zinfo%cycle_time
  
case default
   call erreur("internal error (check_end_cycle)", "unknown time model")

endselect

if (mod(zinfo%iter_loc,itfreq) == 0) call print_info(9,str_w)
!  if (zinfo%end_cycle) call print_info(9,str_w)

open(unit=2001, file="typhon_stop", status="old", iostat=ierr)
if (ierr == 0) then
  zinfo%end_cycle = .true.
  call print_info(9, "INTERRUPTING CYCLE INTEGRATION...")
  close(2001)
endif

!---------------------------------------
endsubroutine check_end_cycle

!------------------------------------------------------------------------------!
! Changes history
!
! may  2006 : subroutine creation (from integrationmacro_zone)
! july 2007 : change writing
!------------------------------------------------------------------------------!

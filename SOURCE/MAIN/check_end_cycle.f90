!------------------------------------------------------------------------------!
! Procedure : end_cycle                           Author  : J. Gressier
!                                                 Created : May 2006
! Fonction 
!   Write cycle information (standard output)
!   and check end cycle
!
!------------------------------------------------------------------------------!
subroutine check_end_cycle(zinfo, dt)
 
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
integer :: ierr

! -- BODY --

select case(zinfo%time_model)

case(time_steady)
  zinfo%residu_ref = max(zinfo%residu_ref, zinfo%cur_res)
  if (zinfo%cur_res/zinfo%residu_ref <= zinfo%residumax) zinfo%end_cycle = .true.
  if (zinfo%iter_loc == zinfo%maxit) zinfo%end_cycle = .true.
case(time_unsteady, time_unsteady_inverse)
  zinfo%cycle_time = zinfo%cycle_time + dt
case default
   call error_stop("internal error (check_end_cycle): unknown time model")
endselect

open(unit=2001, file="typhon_stop", status="old", iostat=ierr)
if (ierr == 0) then
  zinfo%end_cycle = .true.
  call print_master(9, "INTERRUPTING CYCLE INTEGRATION...")
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

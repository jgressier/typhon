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

implicit none

! -- INPUTS --
real(krp), intent(in)        :: dt         ! time step of current iteration

! -- INPUTS/OUTPUTS --
type(st_infozone), intent(inout) :: zinfo      ! zone information (residuals)

! -- Internal variables --
integer :: ierr

! -- BODY --


select case(zinfo%typ_temps)

case(stationnaire)
  zinfo%residu_ref = max(zinfo%residu_ref, zinfo%cur_res)
  if (zinfo%cur_res/zinfo%residu_ref <= zinfo%residumax) zinfo%end_cycle = .true.
  if (mod(zinfo%iter_loc,10) == 0) &
      write(str_w,'(a,i5,a,g10.4)') "    it.",zinfo%iter_loc," | res. = ", log10(zinfo%cur_res)
                                                                       !   log10(zinfo%cur_res/zinfo%residu_ref)
      !if (mod(zinfo%iter_loc,10) == 0) call print_info(9,str_w)

case(instationnaire)
  zinfo%cycle_time = zinfo%cycle_time + dt
  if (mod(zinfo%iter_loc,10) == 0) &      !    if (zinfo%end_cycle) &
      write(str_w,'(a,i5,a,g10.4)') "    it.",zinfo%iter_loc," at time =",zinfo%cycle_time
  
case(periodique)

endselect

if (mod(zinfo%iter_loc,10) == 0) call print_info(9,str_w)
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
!------------------------------------------------------------------------------!


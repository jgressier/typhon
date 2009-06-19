!------------------------------------------------------------------------------!
! Procedure : prb_zone_vol   
!
! Fonction 
!   Computation of volumic probe
!
!------------------------------------------------------------------------------!
subroutine prb_zone_vol(zone, probe)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_PROBE

implicit none

! -- INPUTS --
type(st_zone) :: zone

! -- OUTPUTS --
type(mnu_probe) :: probe

! -- Internal variables --
type(st_grid), pointer               :: pgrid     ! grid pointer

! -- BODY --

select case(probe%type)
case(vol_min)
  probe%result = huge(probe%result)
case(vol_max)
  probe%result = -huge(probe%result)
case(vol_average)
  probe%result = 0._krp
case default
  call error_stop("Internal error (prb_grid_vol): unknown probe type")
endselect

pgrid => zone%gridlist%first  

do while (associated(pgrid)) 

  call prb_grid_vol(zone%defsolver, pgrid, probe)

  pgrid => pgrid%next          ! next grid

enddo
  
! -- MPI reduce --

!call exchange_zonal_timestep(lzone, dt)

select case(probe%type)
case(vol_min, vol_max)
  ! nothing to do
case(vol_average)
  !probe%result = probe%result / zone%info
  call error_stop("VOLUME AVERAGE still needs developement")  
case default
  call error_stop("Internal error (prb_grid_vol): unknown probe type")
endselect


endsubroutine prb_zone_vol

!------------------------------------------------------------------------------!
! change history
!
! June 2009: created
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : exchange_zonal_timestep             Authors : J. Gressier
!                                                 Created : November 2005
! Fonction  
!   Send a Receive global time step for a zone (SEQUENTIAL equivalent)
!
!------------------------------------------------------------------------------!
subroutine exchange_zonal_timestep(zone, dt)

use DEFZONE
use TYPHMAKE
use VARCOM

implicit none

! -- INPUTS --
type(st_zone) :: zone

! -- INPUTS/OUTPUTS --
real(krp)     :: dt      ! time step to send and merge (minimize) with other procs

! -- Internal variables --
integer   :: ierr, ip
real(krp) :: dtmin, val

! -- BODY --

!!! NOTHING TO DO !!!

endsubroutine exchange_zonal_timestep

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
!------------------------------------------------------------------------------!

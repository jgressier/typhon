!------------------------------------------------------------------------------!
! Procedure : exchange_zonal_timestep             Authors : J. Gressier
!                                                 Created : November 2005
! Fonction
!   Send a Receive global time step for a zone
!
!------------------------------------------------------------------------------!
subroutine exchange_zonal_timestep(zone, dt)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG
use MPICOMM
#ifdef MPICOMPIL
use MPICOMM
#endif

implicit none
! -- INPUTS --
type(st_zone) :: zone
! -- INPUTS/OUTPUTS --
real(krp)     :: dt      ! time step to send and merge (minimize) with other procs
! -- Internal variables --
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE), request
real(krp) :: dtmin, val

! -- BODY --

! must access only to zone procs
call allreduce_min_real(dt)

endsubroutine exchange_zonal_timestep
!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
! Feb  2007 : Immediate MPI Send/Receive
!------------------------------------------------------------------------------!

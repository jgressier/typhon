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

implicit none

include 'mpif.h'

! -- INPUTS --
type(st_zone) :: zone

! -- INPUTS/OUTPUTS --
real(krp)     :: dt      ! time step to send and merge (minimize) with other procs

! -- Internal variables --
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE)
real(krp) :: dtmin, val

! -- BODY --

dtmin = huge(dtmin)

do i = 1, zone%info%nbproc
  ip = zone%info%proc(i)
  if (ip /= myprocid) then

    ! -- send to other grid --
    call MPI_SEND(dt, 1, MPI_DOUBLE_PRECISION, ip-1, mpitag_tstep, MPI_COMM_WORLD,  ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to send")

    ! -- receive from other grid --
    call MPI_RECV(val, 1, MPI_DOUBLE_PRECISION, ip-1, mpitag_tstep, MPI_COMM_WORLD, status, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to receive")
    dtmin = min(dtmin, val)

  endif
enddo

dt = min(dt, dtmin)

endsubroutine exchange_zonal_timestep

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
!------------------------------------------------------------------------------!

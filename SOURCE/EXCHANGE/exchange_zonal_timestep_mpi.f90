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
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE), request
real(krp) :: dtmin, val

! -- BODY --

dtmin = huge(dtmin)

do i = 1, zone%info%nbproc
  ip = zone%info%proc(i)
  if (ip /= myprocid) then

    ! -- send to other grid --
    call MPI_ISEND(dt, 1, tympi_real, ip-1, mpitag_tstep, MPI_COMM_WORLD, request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to send")

    call MPI_REQUEST_FREE(request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to free")

    ! -- receive from other grid --
    call MPI_IRECV(val, 1, tympi_real, ip-1, mpitag_tstep, MPI_COMM_WORLD, request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to receive")

    call MPI_WAIT(request , status, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to wait")

    dtmin = min(dtmin, val)

    !print*,myprocid,'send', dt,'to  ',ip
    !print*,myprocid,'recv',val,'from',ip
  endif
enddo

dt = min(dt, dtmin)

endsubroutine exchange_zonal_timestep

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
! Feb  2007 : Immediate MPI Send/Receive
!------------------------------------------------------------------------------!

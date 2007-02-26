!------------------------------------------------------------------------------!
! Procedure : exchange_zonal_residual             Authors : J. Gressier
!                                                 Created : November 2005
! Fonction
!   Send a Receive residual for a zone
!
!------------------------------------------------------------------------------!
subroutine exchange_zonal_residual(info)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG

implicit none

include 'mpif.h'

! -- INPUTS --
type(st_infozone) :: info

! -- INPUTS/OUTPUTS --

! -- Internal variables --
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE), request
real(krp) :: res, sumres, val

! -- BODY --

res    = info%cur_res
sumres = res

do i = 1, info%nbproc

  !-------------------------------------------------------------
  ! merge of residual is done by sum of residual
  ! (same as a residual computation for a single grid)
  !-------------------------------------------------------------

  ip = info%proc(i)

  if (ip /= myprocid) then

    ! -- send to other grid --
    call MPI_ISEND(res, 1, tympi_real, ip-1, mpitag_res, MPI_COMM_WORLD, request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to send")

    call MPI_REQUEST_FREE(request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to free")

    ! -- receive from other grid --
    call MPI_IRECV(val, 1, tympi_real, ip-1, mpitag_res, MPI_COMM_WORLD, request, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to receive")

    call MPI_WAIT(request , status, ierr)
    if (ierr /= 0) call erreur("MPI error", "impossible to wait")

    sumres = sumres + val

  endif
enddo

info%cur_res = sumres

endsubroutine exchange_zonal_residual

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
! Feb  2007 : Immediate MPI Send/Receive
!------------------------------------------------------------------------------!

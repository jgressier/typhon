!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum(value)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG

implicit none

include 'mpif.h'

! -- INPUTS --
type(st_infozone) :: info

! -- INPUTS/OUTPUTS --
real(krp) :: value

! -- Internal variables --
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE), request
real(krp) :: result

! -- BODY --

call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call erreur("MPI error", "impossible to send")

value = result

endsubroutine allreduce_sum

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!

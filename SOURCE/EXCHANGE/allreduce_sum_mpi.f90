!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum_real(value)

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
if (ierr /= 0) call error_stop("MPI error: impossible to send")

value = result

endsubroutine allreduce_sum_real

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum_int(value)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG

implicit none

include 'mpif.h'

! -- INPUTS --
type(st_infozone) :: info

! -- INPUTS/OUTPUTS --
integer(kip) :: value

! -- Internal variables --
integer   :: ierr, ip, i, status(MPI_STATUS_SIZE), request
integer(kip) :: result

! -- BODY --

call MPI_ALLREDUCE(value, result, 1, tympi_int, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to send")

value = result

endsubroutine allreduce_sum_int

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!


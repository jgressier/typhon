!----------------------------------------------------------------------------------------
! MODULE : MPICOMM  
!    
!----------------------------------------------------------------------------------------
module MPICOMM
 
use TYPHMAKE   ! machine accuracy definition
use OUTPUT

include 'mpif.h'

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

integer, private            :: nrequest
integer, private, pointer   :: request(:)
integer, private, parameter :: buffer = 64

contains

!----------------------------------------------------------------------------------------
! initialize request array
!----------------------------------------------------------------------------------------
subroutine init_mpirequest()
implicit none

  if (associated(request)) then
    nrequest = 0
  else
    allocate(request(buffer))
    nrequest = 0
  endif
  
endsubroutine init_mpirequest

!----------------------------------------------------------------------------------------
! add wait request to request array
!----------------------------------------------------------------------------------------
subroutine add_mpirequest(ireq)
implicit none
integer          :: ireq      ! request id to put in stack
integer, pointer :: preq(:)
  if (associated(request)) then
    if (nrequest >= size(request)) then
      allocate(preq(nrequest+buffer))
      preq(1:nrequest) = request(1:nrequest)
      deallocate(request)
      request => preq
    endif
    nrequest = nrequest+1
    request(nrequest) = ireq
  else
    call error_stop("Typhon/Comm error: request array not initialized")
  endif
  
endsubroutine add_mpirequest

!----------------------------------------------------------------------------------------
! wait for all request array
!----------------------------------------------------------------------------------------
subroutine waitall_mpirequest()
implicit none
integer :: ierr

call MPI_WAITALL (nrequest, request(1:nrequest), MPI_STATUSES_IGNORE, ierr)
if (ierr /= 0) call error_stop("Typhon/Comm error: WAITALL error")
nrequest = 0
  
endsubroutine waitall_mpirequest

!------------------------------------------------------------------------------!
!   Sum all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_sum_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr, status(MPI_STATUS_SIZE)
real(krp) :: result
! -- BODY --
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to send")
value = result

endsubroutine allreduce_sum_real

!------------------------------------------------------------------------------!
!   Sum all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_sum_int(value)
implicit none
! -- INPUTS/OUTPUTS --
integer(kip) :: value
! -- Internal variables --
integer      :: ierr, status(MPI_STATUS_SIZE)
integer(kip) :: result
! -- BODY --
call MPI_ALLREDUCE(value, result, 1, tympi_int, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce sum of int")
value = result

endsubroutine allreduce_sum_int

!------------------------------------------------------------------------------!
!   MIN all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_min_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr, status(MPI_STATUS_SIZE)
real(krp) :: result
! -- BODY --
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_MIN, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce min of real")
value = result

endsubroutine allreduce_min_real

!------------------------------------------------------------------------------!
!   MAX all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_max_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr, status(MPI_STATUS_SIZE)
real(krp) :: result
! -- BODY --
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_MAX, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce max of real")
value = result

endsubroutine allreduce_max_real

!------------------------------------------------------------------------------!
!   VOL AVERAGE all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_volavg(vol, avg)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: sumavg, sumvol
! -- Internal variables --
integer   :: ierr, status(MPI_STATUS_SIZE)
real(krp) :: vol, avg
! -- BODY --

call MPI_ALLREDUCE(vol*avg, sumavg, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce vol average (1)")
call MPI_ALLREDUCE(vol, sumvol, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce vol average (2)")
vol = sumvol
avg = sumavg/sumvol

endsubroutine allreduce_volavg

!----------------------------------------------------------------------------------------
endmodule MPICOMM
!------------------------------------------------------------------------------!
! Changes history
! Feb  2014: created
!------------------------------------------------------------------------------!

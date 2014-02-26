!----------------------------------------------------------------------------------------
! MODULE : MPICOMM  
!    
!----------------------------------------------------------------------------------------
module MPICOMM
 
use TYPHMAKE   ! machine accuracy definition
use OUTPUT


! -- Variables globales du module -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

integer, private            :: nrequest
integer, private, pointer   :: request(:)
integer, private, parameter :: buffer = 64

contains

!----------------------------------------------------------------------------------------
! initialize request array
!----------------------------------------------------------------------------------------
integer function request_id()
implicit none
  request_id = nrequest+1
endfunction request_id

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
#ifdef MPICOMPIL
include 'mpif.h'
integer :: i, ierr, status(MPI_STATUS_SIZE, nrequest)
call MPI_WAITALL(nrequest, request, status, ierr)
!if (ierr /= 0) call error_stop("Typhon/Comm error: WAITALL error")
nrequest = 0
#endif
endsubroutine waitall_mpirequest

!------------------------------------------------------------------------------!
!   Sum all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_sum_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr
real(krp) :: result
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to send")
value = result
#endif
endsubroutine allreduce_sum_real

!------------------------------------------------------------------------------!
!   Sum all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_sum_int(value)
implicit none
! -- INPUTS/OUTPUTS --
integer(kip) :: value
! -- Internal variables --
integer      :: ierr
integer(kip) :: result
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ALLREDUCE(value, result, 1, tympi_int, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce sum of int")
value = result
#endif
endsubroutine allreduce_sum_int

!------------------------------------------------------------------------------!
!   MIN all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_min_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr
real(krp) :: result
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_MIN, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce min of real")
value = result
#endif
endsubroutine allreduce_min_real

!------------------------------------------------------------------------------!
!   MAX all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_max_real(value)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: value
! -- Internal variables --
integer   :: ierr
real(krp) :: result
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ALLREDUCE(value, result, 1, tympi_real, MPI_MAX, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce max of real")
value = result
#endif
endsubroutine allreduce_max_real

!------------------------------------------------------------------------------!
!   VOL AVERAGE all data over all threads and broadcast
!------------------------------------------------------------------------------!
subroutine allreduce_volavg(vol, avg)
implicit none
! -- INPUTS/OUTPUTS --
real(krp) :: sumavg, sumvol
! -- Internal variables --
integer   :: ierr
real(krp) :: vol, avg
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ALLREDUCE(vol*avg, sumavg, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce vol average (1)")
call MPI_ALLREDUCE(vol, sumvol, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to reduce vol average (2)")
#endif
vol = sumvol
avg = sumavg/sumvol

endsubroutine allreduce_volavg

!----------------------------------------------------------------------------------------
endmodule MPICOMM
!------------------------------------------------------------------------------!
! Changes history
! Feb  2014: created
!------------------------------------------------------------------------------!

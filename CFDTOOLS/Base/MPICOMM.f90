!----------------------------------------------------------------------------------------
!> @ingroup mpi
!> @brief API to MPI communications
!> - buffer of requests and interface to wait_all
!> - interface to allreduce routines: sum, min, max, volume average
!----------------------------------------------------------------------------------------
module MPICOMM
 
use TYPHMAKE   ! machine accuracy definition
use IOCFD
use CONNECTIVITY

! -- Variables globales du module -------------------------------------------

integer, parameter :: kmpi = 4   ! int kind for tags
integer :: tympi_real            ! int kind for tags
integer :: tympi_int             ! int kind for tags

! -- DECLARATIONS -----------------------------------------------------------

integer, private            :: nrequest
integer, private, pointer   :: request(:)
integer, private, parameter :: buffer = 64

contains

!----------------------------------------------------------------------------------------
!> @brief init module
!----------------------------------------------------------------------------------------
subroutine init_mpicomm()
implicit none
#ifdef MPICOMPIL
include 'mpif.h'
  select case(kip)
  case(4)
    tympi_int = MPI_INTEGER4
  case(8)
    tympi_int = MPI_INTEGER8
  case default
    call cfd_error("MPICOMM init: unexpected typhon integer")
  endselect  
  select case(krp)
  case(4)
    tympi_real = MPI_REAL4
  case(8)
    tympi_real = MPI_REAL8
  case default
    call cfd_error("MPICOMM init: unexpected typhon real")
  endselect 
#endif
endsubroutine 


!!----------------------------------------------------------------------------------------
!!> @brief 
!!----------------------------------------------------------------------------------------
!integer function request_id()
!implicit none
!  request_id = nrequest+1
!endfunction request_id

!----------------------------------------------------------------------------------------
!> @brief initialize request array
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
!> @brief add wait request to request array
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
    call cfd_error("Typhon/Comm error: request array not initialized")
  endif
  
endsubroutine add_mpirequest

!----------------------------------------------------------------------------------------
!> @brief wait for all request array
!----------------------------------------------------------------------------------------
subroutine waitall_mpirequest()
implicit none
#ifdef MPICOMPIL
include 'mpif.h'
integer :: i, ierr, status(MPI_STATUS_SIZE, nrequest)
call MPI_WAITALL(nrequest, request, status, ierr)
!if (ierr /= 0) call cfd_error("Typhon/Comm error: WAITALL error")
nrequest = 0
#endif
end subroutine waitall_mpirequest

!----------------------------------------------------------------------------------------
!> @brief wait for last request and pop array
!----------------------------------------------------------------------------------------
subroutine waitlast_mpirequest()
implicit none
#ifdef MPICOMPIL
include 'mpif.h'
integer :: i, ierr, status(MPI_STATUS_SIZE)
if (nrequest > 0) then
  call MPI_WAIT(request(nrequest), status)
  nrequest = nrequest - 1
endif
#endif
end subroutine waitlast_mpirequest

!------------------------------------------------------------------------------!
!> @brief Sum all data over all threads and broadcast
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
if (ierr /= 0) call cfd_error("MPI error: impossible to send")
value = result
#endif
endsubroutine allreduce_sum_real

!------------------------------------------------------------------------------!
!> @brief   Sum all data over all threads and broadcast
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
if (ierr /= 0) call cfd_error("MPI error: impossible to reduce sum of int")
value = result
#endif
endsubroutine allreduce_sum_int

!------------------------------------------------------------------------------!
!> @brief   MIN all data over all threads and broadcast
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
if (ierr /= 0) call cfd_error("MPI error: impossible to reduce min of real")
value = result
#endif
endsubroutine allreduce_min_real

!------------------------------------------------------------------------------!
!> @brief   MAX all data over all threads and broadcast
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
if (ierr /= 0) call cfd_error("MPI error: impossible to reduce max of real")
value = result
#endif
end subroutine allreduce_max_real

!------------------------------------------------------------------------------!
!> @brief   VOL AVERAGE all data over all threads and broadcast
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
if (ierr /= 0) call cfd_error("MPI error: impossible to reduce vol average (1)")
call MPI_ALLREDUCE(vol, sumvol, 1, tympi_real, MPI_SUM, MPI_COMM_WORLD, ierr)
if (ierr /= 0) call cfd_error("MPI error: impossible to reduce vol average (2)")
#endif
vol = sumvol
avg = sumavg/sumvol

end subroutine allreduce_volavg

!------------------------------------------------------------------------------!
!> @brief Isend int array and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_isend_int(array, n, iproc, tag, wait)
implicit none
! -- INPUTS/OUTPUTS --
integer(kip),   intent(in)    :: n
integer(kip),   intent(in)    :: array(n)
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
logical, optional             :: wait
! -- Internal variables --
integer   :: ierr, irequest
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ISEND(array, n, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
if (present(wait)) then
  if (wait) call waitlast_mpirequest()
endif
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_isend_int)")
#endif
end subroutine 

!------------------------------------------------------------------------------!
!> @brief Irecv int array and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_irecv_int(array, n, iproc, tag, wait)
implicit none
! -- INPUTS --
integer(kip),   intent(in)    :: n
integer(kip),   intent(out)   :: array(n)
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
logical, optional             :: wait
! -- Internal variables --
integer  :: ierr, irequest
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_IRECV(array, n, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
if (present(wait)) then
  if (wait) call waitlast_mpirequest()
endif
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to receive (mpi_irecv_int)")
#endif
end subroutine 

!------------------------------------------------------------------------------!
!> @brief Isend real array and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_isend_real(array, n, iproc, tag, wait)
implicit none
! -- INPUTS/OUTPUTS --
integer(kip),   intent(in)    :: n
real(krp),      intent(in)    :: array(n)
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
logical, optional             :: wait
! -- Internal variables --
integer   :: ierr, irequest
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_ISEND(array, n, tympi_real, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
if (present(wait)) then
  if (wait) call waitlast_mpirequest()
endif
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_isend_real)")
#endif
endsubroutine 

!------------------------------------------------------------------------------!
!> @brief Irecv real array and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_irecv_real(array, n, iproc, tag, wait)
implicit none
! -- INPUTS --
integer(kip),   intent(in)    :: n
real(krp),      intent(out)   :: array(n)
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
logical, optional             :: wait
! -- Internal variables --
integer  :: ierr, irequest
! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'
call MPI_IRECV(array, n, tympi_real, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
if (present(wait)) then
  if (wait) call waitlast_mpirequest()
endif
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to receive (mpi_irecv_real)")
#endif
endsubroutine 

!------------------------------------------------------------------------------!
!> @brief Isend elemc structure and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_isend_elemc(elemc, iproc, tag)
implicit none
! -- INPUTS/OUTPUTS --
type(st_elemc), intent(in)    :: elemc
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
! -- Internal variables --
integer   :: ierr, irequest

! -- BODY --
#ifdef MPICOMPIL
include 'mpif.h'

call MPI_ISEND(elemc%nelem, 1, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_isend_elemc)")

call MPI_ISEND(elemc%elem, elemc%nelem, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_isend_elemc)")
#endif
endsubroutine 

!------------------------------------------------------------------------------!
!> @brief Irecv (and allocate) elemc structure and increase tag
!------------------------------------------------------------------------------!
subroutine mpi_irecv_elemc(elemc, iproc, tag)
implicit none
! -- INPUTS --
type(st_elemc), intent(out)   :: elemc
integer(kip),   intent(in)    :: iproc
integer(kmpi),  intent(inout) :: tag
! -- Internal variables --
#ifdef MPICOMPIL
include 'mpif.h'
integer  :: ierr, irequest, nelem, status(MPI_STATUS_SIZE)
! -- BODY --

call MPI_IRECV(nelem, 1, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_irecv_elemc)")
call MPI_Wait(irequest, status)

call new_elemc(elemc, nelem)
call MPI_IRECV(elemc%elem, elemc%nelem, tympi_int, iproc-1, tag, MPI_COMM_WORLD, irequest, ierr)
call add_mpirequest(irequest)
tag = tag +1
if (ierr /= 0) call cfd_error("MPI error: impossible to send (mpi_irecv_elemc)")

#endif
endsubroutine 


!----------------------------------------------------------------------------------------
endmodule MPICOMM
!------------------------------------------------------------------------------!
! Changes history
! Feb  2014: created
!------------------------------------------------------------------------------!

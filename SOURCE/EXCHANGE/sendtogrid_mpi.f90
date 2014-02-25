!------------------------------------------------------------------------------!
! Procedure : sendtogrid                          Authors : J. Gressier
!                                                 Created : October 2005
! Fonction  
!   Send data to another grid of same zone (MPI version)
!
!------------------------------------------------------------------------------!
subroutine sendtogrid(grid_id, dim, data, tag)

use TYPHMAKE
use VARCOM
use OUTPUT
use COMMTAG
use MPICOMM

implicit none
#ifdef MPICOMPIL
include 'mpif.h'
#endif
! -- INPUTS --
integer(kip)  :: grid_id 
integer(kip)  :: dim
integer(kmpi) :: tag
real(krp)     :: data(dim) 

! -- OUTPUTS --

! -- Internal variables --
integer :: ierr
integer :: irequest

! -- BODY --

#ifdef MPICOMPIL
call MPI_ISEND(data, dim, tympi_real, grid_id-1, tag, MPI_COMM_WORLD, irequest, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to send")
call add_mpirequest(irequest)
#endif

endsubroutine sendtogrid
!------------------------------------------------------------------------------!
! Changes history
! Oct  2005 : created
! Fev  2007 : Immediate MPI Send/Receive
! Feb  2014 : buffered requests
!------------------------------------------------------------------------------!

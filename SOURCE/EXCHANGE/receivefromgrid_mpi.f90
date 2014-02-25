!------------------------------------------------------------------------------!
! Procedure : receivefromgrid                     Authors : J. Gressier
!                                                 Created : October 2005
! Fonction  
!   Receive data from another grid of same zone (MPI version)
!
!------------------------------------------------------------------------------!
subroutine receivefromgrid(grid_id, dim, data, tag)

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

! -- OUTPUTS --
real(krp)    :: data(dim) 

! -- Internal variables --
integer :: ierr
integer :: irequest

! -- BODY --

#ifdef MPICOMPIL
call MPI_IRECV(data, dim, tympi_real, grid_id-1, tag, MPI_COMM_WORLD, irequest, ierr)
if (ierr /= 0) call error_stop("MPI error: impossible to receive")
call add_mpirequest(irequest)
#endif

endsubroutine receivefromgrid
!------------------------------------------------------------------------------!
! Changes history
! Oct  2005 : created
! Fev  2007 : Immediate MPI Send/Receive
! Feb  2014 : buffered requests
!------------------------------------------------------------------------------!

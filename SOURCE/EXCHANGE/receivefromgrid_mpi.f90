!------------------------------------------------------------------------------!
! Procedure : receivefromgrid                     Authors : J. Gressier
!                                                 Created : October 2005
! Fonction  
!   Receive data from another grid of same zone (MPI version)
!
!------------------------------------------------------------------------------!
subroutine receivefromgrid(grid_id, dim, data)

use TYPHMAKE
use VARCOM

implicit none

include 'mpif.h'

! -- INPUTS --
integer(kip) :: grid_id 
integer(kip) :: dim

! -- OUTPUTS --
real(krp)    :: data(dim) 

! -- Internal variables --
integer :: ierr
integer :: status(MPI_STATUS_SIZE)

! -- BODY --

call MPI_RECV(data, dim, MPI_DOUBLE_PRECISION, grid_id-1, 1, MPI_COMM_WORLD, status, ierr)

if (ierr /= 0) call erreur("MPI error", "impossible to receive")

endsubroutine receivefromgrid

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!

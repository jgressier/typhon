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

implicit none

include 'mpif.h'

! -- INPUTS --
integer(kip) :: grid_id 
integer(kip) :: dim
integer(kip) :: tag
real(krp)    :: data(dim) 

! -- OUTPUTS --

! -- Internal variables --
integer :: ierr

! -- BODY --

call MPI_SEND(data, dim, MPI_DOUBLE_PRECISION, grid_id-1, tag, MPI_COMM_WORLD,  ierr)

if (ierr /= 0) call erreur("MPI error", "impossible to send")

endsubroutine sendtogrid

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!

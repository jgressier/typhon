!------------------------------------------------------------------------------!
! Procedure : sendtogrid                          Authors : J. Gressier
!                                                 Created : October 2005
! Fonction  
!   Send data to another grid of same zone (SEQUENTIAL version)
!
!------------------------------------------------------------------------------!
subroutine sendtogrid(grid_id, dim, data)

use TYPHMAKE
use VARCOM

implicit none

!include 'mpif.h'

! -- INPUTS --
integer(kip) :: grid_id 
integer(kip) :: dim
real(krp)    :: data(dim) 

! -- OUTPUTS --

! -- Internal variables --
integer :: ierr

! -- BODY --

call erreur("internal error", "not implemented: should not be used")

endsubroutine sendtogrid

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!

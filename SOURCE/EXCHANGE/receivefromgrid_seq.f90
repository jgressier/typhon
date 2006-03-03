!------------------------------------------------------------------------------!
! Procedure : receivefromgrid                     Authors : J. Gressier
!                                                 Created : October 2005
! Fonction  
!   Receive data from another grid of same zone (SEQUENTIAL version)
!
!------------------------------------------------------------------------------!
subroutine receivefromgrid(grid_id, dim, data)

use TYPHMAKE
use VARCOM

implicit none

! -- INPUTS --
integer(kip) :: grid_id 
integer(kip) :: dim
real(krp)    :: data(dim) 

! -- OUTPUTS --

! -- Internal variables --
integer :: ierr

! -- BODY --

call erreur("internal error", "not implemented: should not be used")


endsubroutine receivefromgrid

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!

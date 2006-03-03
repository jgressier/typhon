!------------------------------------------------------------------------------!
! Procedure : exchange_zonal_residual             Authors : J. Gressier
!                                                 Created : November 2005
! Fonction  
!   Send a Receive residuals for a zone (SEQUENTIAL equivalent)
!
!------------------------------------------------------------------------------!
subroutine exchange_zonal_residual(info)

use DEFZONE
use TYPHMAKE
use VARCOM

implicit none

! -- INPUTS --
type(st_infozone) :: info

! -- INPUTS/OUTPUTS --

! -- Internal variables --
integer   :: ierr, ip
real(krp) :: dtmin, val

! -- BODY --

!!! NOTHING TO DO !!!

endsubroutine exchange_zonal_residual

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2005 : created
!------------------------------------------------------------------------------!

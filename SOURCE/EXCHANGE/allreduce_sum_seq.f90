!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum(value)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG

implicit none

! -- INPUTS --

! -- INPUTS/OUTPUTS --
real(krp) :: value

! -- Internal variables --
real(krp) :: result

! -- BODY --

! nothing to do

endsubroutine allreduce_sum

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!

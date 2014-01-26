!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum_real(value)

use TYPHMAKE

implicit none

! -- INPUTS/OUTPUTS --
real(krp) :: value

! -- BODY --

! nothing to do

endsubroutine allreduce_sum_real

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
! Procedure : allreduce_sum 
! 
! Fonction
!   Sum all data over all threads and broadcast
!
!------------------------------------------------------------------------------!
subroutine allreduce_sum_int(value)

use TYPHMAKE

implicit none

! -- INPUTS/OUTPUTS --
integer(kip) :: value

! -- BODY --

! nothing to do

endsubroutine allreduce_sum_int

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: created
!------------------------------------------------------------------------------!

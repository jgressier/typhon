!----------------------------------------------------------------------------------------
! MODULE : COMMTAG                                   Authors : J. Gressier
!                                                    Created : December 2005
! Fonction
!   Constants for Communication tags (MPI)
!
!----------------------------------------------------------------------------------------
module COMMTAG
 

use TYPHMAKE   ! machine accuracy definition


! -- Variables globales du module -------------------------------------------

integer, parameter :: kmpi = 4

integer(kmpi), parameter   :: mpitag_tstep = 10
integer(kmpi), parameter   :: mpitag_res   = 20
integer(kmpi), parameter   :: mpitag_field = 30
integer(kmpi), parameter   :: mpitag_grad  = 35

! -- DECLARATIONS -----------------------------------------------------------

!contains

!----------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------
endmodule COMMTAG

!------------------------------------------------------------------------------!
! Changes history
!
! Dec  2005 : created
!------------------------------------------------------------------------------!

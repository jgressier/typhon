!----------------------------------------------------------------------------------------
!> @brief enum of MPI tags
!----------------------------------------------------------------------------------------
module COMMTAG

use MPICOMM   ! machine accuracy definition

! -- Variables globales du module -------------------------------------------

integer(kmpi), parameter   :: mpitag_tstep = 10
integer(kmpi), parameter   :: mpitag_res   = 20
integer(kmpi), parameter   :: mpitag_field = 30
integer(kmpi), parameter   :: mpitag_grad  = 35
integer(kmpi), parameter   :: mpitag_grid  = 40

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

!------------------------------------------------------------------------------!
! Procedure : exchange_zonal_residual             Authors : J. Gressier
!                                                 Created : November 2005
! Fonction
!   Send a Receive residual for a zone
!
!------------------------------------------------------------------------------!
subroutine exchange_zonal_residual(info)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG
use MPICOMM

implicit none

! -- INPUTS --
type(st_infozone) :: info

! -- INPUTS/OUTPUTS --

! -- Internal variables --

! -- BODY --

#ifdef MPICOMPIL

!do i = 1, info%nbproc
  !-------------------------------------------------------------
  ! merge of residual is done by sum of residual
  ! (same as a residual computation for a single grid)
  !-------------------------------------------------------------

! must access only to zone procs
call allreduce_sum_real(info%cur_res)

#else
  call error_Stop("Internal error: unexpected MPI routine")
#endif

endsubroutine exchange_zonal_residual
!------------------------------------------------------------------------------!
! Changes history
! Nov  2005 : created
! Feb  2007 : Immediate MPI Send/Receive
!------------------------------------------------------------------------------!

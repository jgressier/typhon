!------------------------------------------------------------------------------!
! Procedure : mpi_strategy_post            Authors : J. Gressier
!                                          Created : September 2005
! Fonction
!   Distribute processors according to user defined strategy
!   without any information about data (mesh sizes)
!------------------------------------------------------------------------------!
subroutine mpi_strategy_post(world)

use TYPHMAKE
use OUTPUT
use MODWORLD
use MODINFO

implicit none

! -- INPUTS/OUTPUTS --
type(st_world) :: world

! -- Internal variables --


! -- BODY --


!--------------------------------------------------------------------
! Initialization of zone parameters

print*,"nothing to do"
!call print_info(5,"* Initialisation des zones")
!do izone = 1, world%prj%nzone!
!
!enddo



endsubroutine mpi_strategy_post

!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005 : created
!------------------------------------------------------------------------------!

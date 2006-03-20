!------------------------------------------------------------------------------!
! Procedure : mpi_strategy_pre             Authors : J. Gressier
!                                          Created : September 2005
! Fonction
!   Distribute processors according to user defined strategy
!   without any information about data (mesh sizes)
!------------------------------------------------------------------------------!
subroutine mpi_strategy_pre(world)

use TYPHMAKE
use OUTPUT
use MODWORLD
use MODINFO

implicit none

! -- INPUTS/OUTPUTS --
type(st_world) :: world

! -- Internal variables --
integer(kip) :: izone, ip

! -- BODY --


!--------------------------------------------------------------------
! Initialization of zone parameters

allocate(world%info%headproc(world%prj%nzone))

do izone = 1, world%prj%nzone
  world%info%headproc(izone) = izone                    ! each zone has a differente master proc
  world%zone(izone)%info%nbproc = world%info%nbproc     ! each zone uses all procs
  allocate(world%zone(izone)%info%proc(world%zone(izone)%info%nbproc))
  do ip = 1, world%zone(izone)%info%nbproc
    world%zone(izone)%info%proc(ip) = ip
  enddo
enddo



endsubroutine mpi_strategy_pre

!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005 : created
!------------------------------------------------------------------------------!

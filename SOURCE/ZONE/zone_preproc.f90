!------------------------------------------------------------------------------!
! Procedure : zone_preproc
!
! Fonction
!   Mesh pre-processing
!------------------------------------------------------------------------------!
subroutine zone_preproc(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MESHPARAMS

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Private Data --
integer                :: izone
type(st_grid), pointer :: pgrid

! -- BODY --

zone%info%totvolume = 0._krp

pgrid => zone%gridlist%first

do while (associated(pgrid))

  call grid_preproc(zone%defsolver, pgrid)

  zone%info%totvolume = zone%info%totvolume + pgrid%info%volume
  
  pgrid => pgrid%next
enddo

! -- reduce (sum) on all threads
call allreduce_sum(zone%info%totvolume)

endsubroutine zone_preproc
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
! Dec  2010: from readallmesh, all mesh pre-processing
! Apr  2011: loop on grids and split to grid_preproc
! Feb  2013: moved (called by) to init_world
!------------------------------------------------------------------------------!

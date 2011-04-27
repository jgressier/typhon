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

pgrid => zone%gridlist%first

do while (associated(pgrid))

  call grid_preproc(zone%defsolver%defmesh, pgrid)

  pgrid => pgrid%next
enddo

  
endsubroutine zone_preproc
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
! Dec  2010: from readallmesh, all mesh pre-processing
! Apr  2011: loop on grids and split to grid_preproc
!------------------------------------------------------------------------------!

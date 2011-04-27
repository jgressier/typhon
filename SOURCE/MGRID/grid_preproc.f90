!------------------------------------------------------------------------------!
! Procedure : grid_preproc
!
! Fonction
!   Mesh pre-processing
!------------------------------------------------------------------------------!
subroutine grid_preproc(defmesh, grid)

use TYPHMAKE
use OUTPUT
use MGRID
use MESHPARAMS

implicit none

! -- INPUTS/OUTPUTS --
type(mnu_mesh) :: defmesh

! -- INPUTS/OUTPUTS --
type(st_grid) :: grid

! -- Private Data --
logical                :: facetag

! -- BODY --

facetag = ( defmesh%splitmesh /= split_none )

call create_face_connect(facetag, grid%umesh)

  
endsubroutine grid_preproc
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation from zone_preproc
!------------------------------------------------------------------------------!

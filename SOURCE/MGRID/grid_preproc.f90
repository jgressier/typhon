!------------------------------------------------------------------------------!
! Procedure : grid_preproc
!
! Fonction
!   Mesh pre-processing
!------------------------------------------------------------------------------!
subroutine grid_preproc(defsolver, grid)

use TYPHMAKE
use OUTPUT
use MGRID
use MENU_SOLVER
use MESHPARAMS
use MESHCONNECT
use COLOR

implicit none

! -- INPUTS/OUTPUTS --
type(mnu_solver) :: defsolver

! -- INPUTS/OUTPUTS --
type(st_grid) :: grid

! -- Private Data --
logical                :: facetag

! -- BODY --

select case(grid%info%gridtype)

case(grid_ust)

  facetag = ( defsolver%defmesh%defsplit%splitmesh /= split_none )
  call print_info(10,"* compute FACE connectivity")
  call create_face_connect(facetag, grid%umesh, verbose=1)
  call grid_ustpreproc(defsolver, grid)
  grid%info%volume = sum(grid%umesh%mesh%volume(1:grid%umesh%ncell_int, 1, 1))
  call print_info(10,"* compute FACE colors")
  call coo_colorgroup(grid%umesh%nface, grid%umesh%ncell, grid%umesh%facecell, grid%umesh%colors)
  call print_info(12,"  . "//trim(strof(grid%umesh%colors%nbnodes))//" colors")
  grid%info%ndof = grid%umesh%ncell !! * grid%ndof
  
case(grid_str)

  ! STRUCTURED MESH PREPROCESSING
  ! - no connectivity -> implicit
  if (grid%strmesh%nk == 0) then
    grid%info%volume = grid%strmesh%lx*grid%strmesh%ly
  else
    grid%info%volume = grid%strmesh%lx*grid%strmesh%ly*grid%strmesh%lz
  endif
      
case default
  call error_stop("unknown type of grid (grid_preproc)")
endselect
  
endsubroutine grid_preproc
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation from zone_preproc
!------------------------------------------------------------------------------!
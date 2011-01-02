!------------------------------------------------------------------------------!
! Procedure : readallmesh                 Auteur : J. Gressier
!
! Fonction
!   Import all mesh
!------------------------------------------------------------------------------!
subroutine readallmesh(world)

use TYPHMAKE
use OUTPUT
use MODWORLD

implicit none

! -- INPUTS/OUTPUTS --
type(st_world) :: world

! -- Private Data --
integer                :: izone
type(st_grid), pointer :: pgrid

! -- BODY --

do izone = 1, world%prj%nzone

  pgrid => add_grid(world%zone(izone)%gridlist)

  select case(world%zone(izone)%defsolver%defmesh%format)

  case(fmt_CGNS)
    call importcgns_mesh(world%zone(izone)%defsolver%defmesh, pgrid%umesh)

  case(fmt_TYPHON)
    !call importtyphon_mesh(world%zone(izone))
    call error_stop("reading mesh: TYPHON format not implemented")

  case default
    call error_stop("reading mesh: unknown mesh format")
  endselect

  !--------------------------------

  call zone_preproc(world%zone(izone))

enddo

endsubroutine readallmesh
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
!------------------------------------------------------------------------------!

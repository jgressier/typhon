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
integer :: izone

! -- BODY --

do izone = 1, world%prj%nzone

  select case(world%zone(izone)%defsolver%defmesh%format)

  case(fmt_CGNS)
    call importcgns_mesh(world%zone(izone))

  case(fmt_TYPHON)
    call importtyphon_mesh(world%zone(izone))

  case default
    call erreur("reading mesh","unknown mesh format")
  endselect

  !--------------------------------
  
enddo


endsubroutine readallmesh
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
!------------------------------------------------------------------------------!

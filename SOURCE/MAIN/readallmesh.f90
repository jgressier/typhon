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

!DEC$ IF DEFINED (CGNS)
  case(fmt_CGNS)
    pgrid%info%gridtype = grid_ust
    call importcgns_mesh(world%zone(izone)%defsolver%defmesh, pgrid%umesh)
!DEC$ ENDIF 

  case(fmt_TYPHON)
    pgrid%info%gridtype = grid_ust
    call importtyphon_mesh(world%zone(izone)%defsolver%defmesh, pgrid%umesh)

  case(fmt_AUTOBLOCK)
    pgrid%info%gridtype = grid_str
    call create_autoblockmesh(world%zone(izone)%defsolver%defmesh, pgrid%strmesh)
    
  case default
    call error_stop("reading mesh: unknown mesh format")
  endselect

enddo

endsubroutine readallmesh
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
! Feb  2013: add structured block auto-blocking
!------------------------------------------------------------------------------!

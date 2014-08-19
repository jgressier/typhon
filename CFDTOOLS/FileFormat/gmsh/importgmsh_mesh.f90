!------------------------------------------------------------------------------!
! Procedure : importgmsh_mesh
!
! Fonction: GMSH mesh reading
!
!------------------------------------------------------------------------------!
subroutine importgmsh_mesh(defmesh, umesh)

use IO_UNIT
use GMSH
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh)   , intent(in)  :: defmesh

! -- OUTPUTS --
type(st_ustmesh) , intent(out) :: umesh

! -- Internal variables --
!integer             :: iunit
type(st_defgmsh)    :: defgmsh

! -- BODY --

!------------------------------------------------------------------------
! read GMSH mesh
!------------------------------------------------------------------------

call cfd_print("* READING GMSH MESH: "//trim(defmesh%filename))

! --- open GMSH file ---
!iunit = getnew_io_unit()
call gmsh_openread(trim(defmesh%filename), defgmsh)

! --- process GMSH file ---
call gmshread_ustmesh(defgmsh, umesh)

! --- close GMSH file ---
call gmsh_close(defgmsh)

endsubroutine importgmsh_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation (read GMSH mesh file)
!------------------------------------------------------------------------------!

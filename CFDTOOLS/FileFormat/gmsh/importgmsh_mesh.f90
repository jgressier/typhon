!------------------------------------------------------------------------------!
! Procedure : importgmsh_mesh
!
! Fonction: MESH reading
!
!------------------------------------------------------------------------------!
subroutine importgmsh_mesh(defmesh, umesh)

use IO_UNIT
use GMSH
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh) :: defmesh

! -- OUTPUTS --
type(st_ustmesh) :: umesh

! -- Internal variables --
integer               :: iunit
type(st_defgmsh)      :: defgmsh

! -- BODY --

call cfd_print("* READING GMSH MESH: "//trim(defmesh%filename))

!------------------------------
! open xbin file

iunit = getnew_io_unit()
call gmsh_openread(iunit, trim(defmesh%filename), defgmsh)

call gmshread_ustmesh(defgmsh, umesh)

endsubroutine importgmsh_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation (read gmsh internal format mesh file)
!------------------------------------------------------------------------------!

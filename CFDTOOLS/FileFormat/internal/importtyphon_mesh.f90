!------------------------------------------------------------------------------!
! Procedure : importtyphon_mesh
!
! Fonction: MESH reading
!
!------------------------------------------------------------------------------!
subroutine importtyphon_mesh(defmesh, umesh)

use IO_UNIT
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh) :: defmesh

! -- OUTPUTS --
type(st_ustmesh) :: umesh

! -- Internal variables --
integer                       :: iunit
type(st_deftyphon)            :: deftyphon
type(st_xbindatasection)      :: xbindata

! -- BODY --

call cfd_print("* READING TYPHON INTERNAL MESH: "//trim(defmesh%filename))

!------------------------------
! open xbin file

iunit = getnew_io_unit()
call typhon_openread(iunit, trim(defmesh%filename), deftyphon)

call typhonread_ustmesh(deftyphon%defxbin, umesh)

endsubroutine importtyphon_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation (read TYPHON internal format mesh file)
!------------------------------------------------------------------------------!

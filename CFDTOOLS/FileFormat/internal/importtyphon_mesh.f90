!------------------------------------------------------------------------------!
! Procedure : importtyphon_mesh
!
! Fonction: TYPHON internal mesh reading
!
!------------------------------------------------------------------------------!
subroutine importtyphon_mesh(defmesh, umesh)

! use IO_UNIT
! use XBIN_IO
use TYPHON_FMT
! use TYFMT_MESH
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh)   , intent(in)  :: defmesh

! -- OUTPUTS --
type(st_ustmesh) , intent(out) :: umesh

! -- Internal variables --
! integer             :: iunit
type(st_deftyphon)  :: deftyphon

! -- BODY --

!------------------------------------------------------------------------
! read TYPH mesh
!------------------------------------------------------------------------

call cfd_print("* READING TYPHON INTERNAL MESH: "//trim(defmesh%filename))

! --- open TYPHON internal file ---
! iunit = getnew_io_unit() ! (defined in defmesh)
call typhon_openread(trim(defmesh%filename), deftyphon)

! --- process TYPHON internal file ---
call typhonread_ustmesh(deftyphon, umesh)

! --- close TYPHON internal file ---
call typhon_close(deftyphon)

endsubroutine importtyphon_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Apr  2011: creation (read TYPHON internal format mesh file)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : importcgns_mesh
!
! Fonction: CGNS mesh reading
!
!------------------------------------------------------------------------------!
subroutine importcgns_mesh(defmesh, umesh)

!use IO_UNIT
use CGNS_STRUCT
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh)   , intent(in)  :: defmesh

! -- OUTPUTS --
type(st_ustmesh) , intent(out) :: umesh

! -- Internal variables --
integer             :: iunit
integer             :: ierr

! -- BODY --

!------------------------------------------------------------------------
! read CGNS mesh
!------------------------------------------------------------------------

call cfd_print("* READING CGNS MESH: "//trim(defmesh%filename))

! --- open CGNS file ---
! iunit = getnew_io_unit() ! (defined by cg_open_f)
call cg_open_f(trim(defmesh%filename), MODE_READ, iunit, ierr)
if (ierr /= 0) call cfd_error("(CGNS IO) cannot open CGNS file "//trim(defmesh%filename))

! --- process CGNS file ---
call readcgnszone(iunit, defmesh%icgnsbase, defmesh%icgnszone, umesh)

! --- close CGNS file ---
call cg_close_f(iunit, ierr)

endsubroutine importcgns_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation (read CGNS mesh file)
! Feb  2004: read TYPHMSH format mesh
! June 2010: lectzone_mesh.f90->importcgns_mesh.f90
! Dec  2010: inline readcgnsfile, simplify to unique base reading
!------------------------------------------------------------------------------!

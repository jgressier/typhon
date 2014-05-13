!------------------------------------------------------------------------------!
! Procedure : importcgns_mesh
!
! Fonction: MESH reading
!
!------------------------------------------------------------------------------!
subroutine importcgns_mesh(defmesh, umesh)

use USTMESH
use MESHPARAMS
use CGNS_STRUCT

implicit none

! -- INPUTS --
type(mnu_mesh) :: defmesh

! -- OUTPUTS --
type(st_ustmesh) :: umesh

! -- Internal variables --
integer                :: unit, ier, i

! -- BODY --

!------------------------------------------------------------------------
! read CGNS mesh
!------------------------------------------------------------------------

call cfd_print("READING CGNS MESH: "//trim(defmesh%filename))

! --- open CGNS file ---
#ifdef CGNS
call cg_open_f(trim(defmesh%filename), MODE_READ, unit, ier)

if (ier /= 0) call cfd_error("(CGNS IO) cannot open CGNS file "//trim(defmesh%filename))
   
call readcgnszone(unit, defmesh%icgnsbase, defmesh%icgnszone, umesh)

call check_ustmesh_elements(umesh)

! --- CGNS close ---
call cg_close_f(unit, ier)

#else
  call cfd_error("CGNS has not been activated during compilation")
#endif

endsubroutine importcgns_mesh
!------------------------------------------------------------------------------!
! Change history
!
! nov  2002: creation (read CGNS file)
! fev  2004: read TYPHMSH format mesh
! June 2010: lectzone_mesh.f90->importcgns_mesh.f90
! Dec  2010: inline readcgnsfile, simplify to unique base reading
!------------------------------------------------------------------------------!

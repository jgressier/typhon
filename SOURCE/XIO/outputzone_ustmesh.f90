!------------------------------------------------------------------------------!
! Procedure : outputzone_ustmesh
!
! Function
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_ustmesh(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN
use TYFMT_MESH
use VTKMESH
use CGNS_STRUCT

implicit none

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer                :: dim, ufc, ir, izone, iunit, nbmesh, nbsol
integer                :: info
type(st_ustmesh), pointer :: p_umesh
type(st_genericfield)  :: vfield
type(st_deftyphon)     :: deftyphon2
integer                :: isize(3)     ! info array for zone

! -- BODY --

select case(defio%meshlevel)
case(meshlevel_current)
  p_umesh => zone%gridlist%first%umesh
case(meshlevel_legacy)
  p_umesh => zone%gridlist%first%umesh_legacy
case default
  call error_stop("Internal error (outputzone_ustmesh): unknown mesh level parameter")
endselect

select case(defio%format)

case(fmt_TYPHON)

  if (((defio%meshdef == mesh_shared).or.(defio%meshdef == mesh_sharedcon)).and.(.not.defio%savedmesh)) then
    call print_info(4,"  write TYPHON shared mesh: "//trim(defio%basename)//trim("."//xtyext_mesh))
    nbmesh = 1
    nbsol  = 0
    call typhon_openwrite(trim(defio%basename)//trim("."//xtyext_mesh), deftyphon2, nbmesh, nbsol, mesh_full)
    call typhonwrite_ustmesh(deftyphon2, p_umesh)
    call typhon_close(deftyphon2)
    defio%savedmesh = .true.
  endif

  call typhonwrite_ustmesh(defio%deftyphon, p_umesh, trim(defio%basename)//trim("."//xtyext_mesh))

case(fmt_TECPLOT)
  call error_stop("(Internal error) Unable to use general output with TECPLOT format")

case(fmt_VTK, fmt_VTKBIN)
  call writevtk_ustmesh(defio%defvtk, p_umesh)

#ifdef CGNS
case(fmt_CGNS, fmt_CGNS_linked)

  isize(1) = p_umesh%nvtex       ! vertex size
  isize(2) = p_umesh%ncell_int   ! cell size
  isize(3) = 0                       ! boundary vertex size (zero if vertices not sorted)

  ! -- create CGNS zone --

  call cg_zone_write_f(defio%iunit, defio%izone, "ustmesh", isize, Unstructured, izone, info)
    ! izone is an output
  if (info /= 0) &
    call error_stop("CGNS IO error: writing  CGNS zone header (error "//trim(strof(info))//")")

  ! -- write TYPHON ustmesh to CGNS zone (defio%izone is a CGNS base index) --

  call writecgns_ustmesh(defio%iunit, defio%izone, izone, p_umesh)

  call writecgns_bocomesh(defio%iunit, defio%izone, izone, p_umesh)
#endif/*CGNS*/

case default
  call error_stop("Internal error (outputzone_ustmesh): unknown output format parameter")
endselect

endsubroutine outputzone_ustmesh
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009: created
! Sept  2011: save legacy or current mesh
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Routine: gmshread_ustmesh
!
! Read a GMSH format to USTMESH structure
!------------------------------------------------------------------------------!
subroutine gmshread_ustmesh(defgmsh, umesh)

use IO_UNIT
use GMSH
use USTMESH

implicit none

! -- INPUTS --
type(st_defgmsh)       :: defgmsh
type(st_ustmesh)         :: umesh

! -- OUTPUTS --

! -- private data --
integer       :: info, iunit
logical       :: nodes, elements, marks
character(len=gmshlen) :: sectionline

! -- BODY --

call init_ustmesh(umesh, 1)   ! default values initialization

nodes    = .false.
elements = .false.
marks    = .false.

do while (.not.(elements.and.nodes.and.marks))

  call gmshread_section(defgmsh, sectionline)
  
  select case(sectionline)
  case("$Nodes") ! ----- read NODES -----
    if (nodes) call cfd_error("GMSH: too many nodes section")
    call cfd_print("> nodes coordinates")
    call gmshread_nodes(defgmsh, umesh%mesh)
    nodes = .true.
    umesh%nvtex = umesh%mesh%nvtex

  case("$Elements") ! ----- read ELEMENTS -----
    if (nodes) call cfd_error("GMSH: too many elements section")
    call cfd_print("> cell elements")
    call gmshread_elemvtex(defgmsh, umesh%cellvtex)
    elements = .true.

  case("$PhysicalNames") ! ----- read ELEMENTS -----
    if (nodes) call cfd_error("GMSH: too many elements section")
    call cfd_print("> marks")
    !call createboco(umesh, deftymesh2%nfacemark)
    !
    !do ib = 1, deftymesh2%nfacemark
    !  call gmshread_bcmark(defgmsh2%defxbin, umesh%boco(ib))
    !enddo
  case default
    call cfd_print("  . unused section "//trim(sectionline))
  endselect
enddo

call check_ustmesh_elements(umesh)

endsubroutine gmshread_ustmesh
!------------------------------------------------------------------------------!
! Change history
! Apr 2011: created
! May 2011: able to read shared mesh
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Routine: gmshread_ustmesh
!
! Function
!   Read a GMSH format to USTMESH structure
!------------------------------------------------------------------------------!
subroutine gmshread_ustmesh(defgmsh, umesh)

use IO_UNIT
use GMSH
use USTMESH

implicit none

! -- INPUTS --
type(st_defgmsh) , intent(inout) :: defgmsh

! -- OUTPUTS --
type(st_ustmesh) , intent(out)   :: umesh

! -- Internal variables --
integer       :: info, iunit
logical       :: nodes, elements, marks, eof
character(len=gmshlen) :: sectionline

! -- BODY --

!-----------------------------------------------------------------
! Initialization of USTMESH

call init_ustmesh(umesh, 1)   ! default values initialization

nodes    = .false.
elements = .false.
marks    = .false.

do while (.not.eof)
  call gmshread_section(defgmsh, sectionline)

  select case(sectionline)
  case("$Nodes") ! ----- read NODES -----
    if (nodes) call cfd_error("GMSH: too many nodes section")
    call cfd_print("> nodes coordinates")
    call gmshread_nodes(defgmsh, umesh%mesh)
    nodes = .true.
    umesh%nvtex = umesh%mesh%nvtex

  case("$Elements") ! ----- read ELEMENTS -----
    if (elements) call cfd_error("GMSH: too many elements section")
    call cfd_print("> cell elements")
    call gmshread_elemvtex(defgmsh, umesh)
    elements = .true.

  case("$PhysicalNames") ! ----- read MARKS -----
    if (marks) call cfd_error("GMSH: too many elements section")
    call cfd_print("> marks")
    call gmshread_phynames(defgmsh)
    marks = .true.

  case("EOF") ! end of file
    eof = .true.

  case default
    call cfd_print("  . unused section "//trim(sectionline))
    call gmshseek_section(defgmsh, trim(sectionline))
  endselect
enddo

if (.not.(elements.and.nodes)) then   ! marks are not necessary
  call cfd_error("GMSH import: missing elements or nodes")
endif

call cfd_print("> match boundary condition and marks")
call gmsh_match_phynames(defgmsh, umesh)

call check_ustmesh_elements(umesh)

endsubroutine gmshread_ustmesh
!------------------------------------------------------------------------------!
! Change history
! Apr 2011: created
! May 2011: able to read shared mesh
!------------------------------------------------------------------------------!

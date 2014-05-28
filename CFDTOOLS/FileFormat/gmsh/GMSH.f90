!------------------------------------------------------------------------------!
! MODULE : GMSH
!
!------------------------------------------------------------------------------!
module GMSH

use MESHPREC         ! Precision configuration
use USTMESH
USE STRING
use IO_UNIT

implicit none

! -- Global Variables -------------------------------------------

integer,  parameter :: gmsh_maxver = 22
integer,  parameter :: gmshlen     = 256   ! string length

character(len=4),  parameter :: gmsh_ext    = "gmsh"

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! CONSTANTS for GMSH ELEMENT TYPE
!------------------------------------------------------------------------------!
integer(kpp), parameter :: gmsh_maxindex = 93

integer(kpp), parameter :: gmsh_node1   = 15
integer(kpp), parameter :: gmsh_bar2    = 1
integer(kpp), parameter :: gmsh_bar3    = 8
integer(kpp), parameter :: gmsh_bar4    = 26
integer(kpp), parameter :: gmsh_bar5    = 27
integer(kpp), parameter :: gmsh_tri3    = 2
integer(kpp), parameter :: gmsh_tri6    = 9
integer(kpp), parameter :: gmsh_tri9    = 20
integer(kpp), parameter :: gmsh_tri10   = 21
integer(kpp), parameter :: gmsh_tri12   = 22
integer(kpp), parameter :: gmsh_tri15   = 23
integer(kpp), parameter :: gmsh_quad4   = 3
integer(kpp), parameter :: gmsh_quad8   = 16
integer(kpp), parameter :: gmsh_quad9   = 10
integer(kpp), parameter :: gmsh_tetra4  = 4
integer(kpp), parameter :: gmsh_tetra10 = 11
integer(kpp), parameter :: gmsh_tetra20 = 29
integer(kpp), parameter :: gmsh_tetra35 = 30
integer(kpp), parameter :: gmsh_hexa8   = 5
integer(kpp), parameter :: gmsh_hexa20  = 17
integer(kpp), parameter :: gmsh_hexa27  = 12
integer(kpp), parameter :: gmsh_hexa64  = 92
integer(kpp), parameter :: gmsh_hexa125 = 93
integer(kpp), parameter :: gmsh_penta6  = 6
integer(kpp), parameter :: gmsh_penta15 = 18
integer(kpp), parameter :: gmsh_penta18 = 13
integer(kpp), parameter :: gmsh_pyra5   = 7
integer(kpp), parameter :: gmsh_pyra14  = 14
integer(kpp), parameter :: gmsh_pyra13  = 19

! -- Private data --

integer, private :: maxnbnodes = 125
!integer, private :: gmsh_elemnbnodes(1:gmsh_maxindex)
!integer, private :: gmsh_elemorder(1:gmsh_maxindex)

! -- Parameters --

!------------------------------------------------------------------------------!
! ST_DEFGMSH
!------------------------------------------------------------------------------!
type st_defgmsh
  integer :: version
  integer :: iunit, rsize
  logical :: binary
  integer :: nnodes, nelem, nphy
  integer :: maxivtex, maxielem       ! maximum index of node and elem
endtype st_defgmsh

!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! open GMSH file
!------------------------------------------------------------------------------!
subroutine gmsh_openread(iunit, filename, defgmsh)
implicit none
! -- INPUTS --
integer            :: iunit
character(len=*)   :: filename
! -- OUTPUTS --
type(st_defgmsh) :: defgmsh
! -- private data --
integer :: info
! -- BODY --

  open(unit=iunit, file=trim(filename), form='formatted', iostat = info)
  if (info /= 0) &
    call cfd_error("GMSH: unable to open file "//trim(filename))
  call gmshread_filedef(defgmsh)
  if (defgmsh%version > gmsh_maxver) &
    call cfd_error("GMSH: unable to handle this GMSH format version number")

endsubroutine gmsh_openread

!------------------------------------------------------------------------------!
! read gmsh header definition
!------------------------------------------------------------------------------!
recursive subroutine gmshseek_section(defgmsh, section)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defgmsh)       :: defgmsh
character(len=*)       :: section
! -- private data --
integer                :: info
character(len=gmshlen) :: line
logical                :: found
! -- BODY --

found = .false.
do while (.not.found)
  read(defgmsh%iunit,*) line 
  if (samestring(line, section)) then
    found = .true.
    print*,'gmsh found '//trim(section)
    exit
  elseif (samestring(line, '$Comments')) then
    print*,'gmsh comment section'
    do while (.not.samestring(line, '$EndComment'))
      read(defgmsh%iunit,*) line
      if (io_eof(defgmsh%iunit)) call cfd_error("GMSH: unexpected end of file in comment section")
    enddo
  elseif (samestring(line, '')) then   ! empty line
    ! nothing to do
  endif
  if (io_eof(defgmsh%iunit)) call cfd_error("GMSH: unexpected end of file")
enddo
if (.not.found) call cfd_error("GMSH: bad header, expecting "//section)

endsubroutine gmshseek_section

!------------------------------------------------------------------------------!
! read gmsh header definition
!------------------------------------------------------------------------------!
subroutine gmshread_section(defgmsh, section)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defgmsh)                    :: defgmsh
character(len=gmshlen), intent(out) :: section
! -- private data --
integer                :: info
character(len=gmshlen) :: line
logical                :: found
! -- BODY --

found = .false.
do while (.not.found)
  read(defgmsh%iunit,*) line 
  if (samestring(line, '$Comments')) then
    print*,'gmsh comment section'
    do while (.not.samestring(line, '$EndComment'))
      read(defgmsh%iunit,*) line
      if (io_eof(defgmsh%iunit)) call cfd_error("GMSH: unexpected end of file in comment section")
    enddo
  elseif (line(1:1) == '$') then 
    found = .true.
  elseif (samestring(line, '')) then   ! empty line
    ! nothing to do
  endif
  if (io_eof(defgmsh%iunit)) call cfd_error("GMSH: unexpected end of file")
enddo
if (found) then
  section = line
else
  call cfd_error("GMSH: no section found")
endif
endsubroutine gmshread_section

!------------------------------------------------------------------------------!
! read gmsh header definition
!------------------------------------------------------------------------------!
subroutine gmshread_filedef(defgmsh)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defgmsh)       :: defgmsh
! -- private data --
integer                :: info, ftype, kprec
character(len=gmshlen) :: line
character(len=10)      :: word
! -- BODY --

call gmshseek_section(defgmsh, '$MeshFormat')
read(defgmsh%iunit,*) line

call nthword(1, line, word, info, ' ')
select case(word)
case('2')
  defgmsh%version = 20
case('2.2')
  defgmsh%version = 22
case default
  call cfd_error("GMSH: unknown version number "//trim(word))
endselect

call nthword(2, line, word, info, ' ')
select case(word)
case('0')
  defgmsh%binary = .false.
case('1')
  defgmsh%version = .true.
  call cfd_error("GMSH: unable to read binary file")
case default
  call cfd_error("GMSH: unknown file type "//trim(word))
endselect

call nthword(3, line, word, info, ' ')
select case(word)
case('4')
  defgmsh%rsize = 4
  call cfd_error("GMSH: only 8-bytes real expected")
case('8')
  defgmsh%rsize = 8
case default
  call cfd_error("GMSH: unknown data size "//trim(word))
endselect

call gmshseek_section(defgmsh, '$EndMeshFormat')

endsubroutine gmshread_filedef

!------------------------------------------------------------------------------!
! read gmsh NODES
!------------------------------------------------------------------------------!
subroutine gmshread_nodes(defgmsh, mesh)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defgmsh)       :: defgmsh
type(st_mesh)          :: mesh
! -- private data --
integer                   :: info, i
character(len=gmshlen)    :: line
integer(kip), allocatable :: ivtex(:)
real(8),      allocatable :: vtex(:,:)
! -- BODY --

!call gmshread_section(defgmsh, '$Nodes')
read(defgmsh%iunit,*) defgmsh%nnodes
if (defgmsh%nnodes < 3) call cfd_error("GMSH: not enough nodes")

mesh%nvtex = defgmsh%nnodes
allocate(mesh%vertex(mesh%nvtex, 1, 1))
allocate(vtex(3,1))
allocate(ivtex(mesh%nvtex))

do i = 1, mesh%nvtex
  read(defgmsh%iunit,*) ivtex(i), vtex
  if (ivtex(i) <= mesh%nvtex) then
    mesh%vertex(ivtex(i),1,1)%x = vtex(1,1) 
    mesh%vertex(ivtex(i),1,1)%y = vtex(2,1) 
    mesh%vertex(ivtex(i),1,1)%z = vtex(3,1) 
  else
    call cfd_error("GMSH: vertex sparse filling not allowed")
  endif
enddo
defgmsh%maxivtex = maxval(ivtex)

deallocate(vtex, ivtex)

call gmshseek_section(defgmsh, '$EndNodes')

endsubroutine gmshread_nodes


!------------------------------------------------------------------------------!
! read gmsh ELEMENTS
!------------------------------------------------------------------------------!
subroutine gmshread_elemvtex(defgmsh, elemvtex)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defgmsh)      :: defgmsh
type(st_genelemvtex)  :: elemvtex        ! CELL/FACE -> VTEX connectivity
! -- private data --
integer, parameter        :: maxntag  = 3
integer, parameter        :: maxnnode = 27   ! second order hexa
integer                   :: info, i, ic, it, n, nnode, ntag, null(10), maxtag
character(len=gmshlen)    :: line
integer(kip), allocatable :: ielem(:), itype(:), itag(:,:), elem(:,:), nelem(:), maxelem
integer(kip)              :: cell(maxnnode)

! -- BODY --

!call gmshread_section(defgmsh, '$Elements')
read(defgmsh%iunit,*) defgmsh%nelem
if (defgmsh%nelem < 4) call cfd_error("GMSH: not enough elements")

allocate(ielem(defgmsh%nelem))
allocate(itype(defgmsh%nelem))
allocate( itag(defgmsh%nelem, maxntag))  ; itag = -1
allocate( elem(defgmsh%nelem, maxnnode)) ; elem = 0

! --- read GMSH elements ---

do i = 1, defgmsh%nelem
  read(defgmsh%iunit,*) line
  read(line,*) ielem(i), itype(i), ntag
  nnode = nvtex_gmshelement(itype(i))
  if (ntag  >  maxntag) call cfd_error("GMSH: too many tags")
  if (nnode <= 0)       call cfd_error("GMSH: unexpected number of nodes per element")
  read(line,*) null(1:3), itag(i, 1:ntag), elem(i, 1:min(maxnnode, nnode))
  ! -- filter to order of elements --
  ! TODO
enddo
defgmsh%maxielem = maxval(ielem)

! --- create USTMESH internal elements ---

maxelem =  maxval(itype)
allocate(nelem(maxelem))
do it = 1, maxelem
  nelem(it) = count(itype==it)
  if (nelem(it) /= 0) then
    call addelem_genelemvtex(elemvtex)
    call new_elemvtex(elemvtex%elem(elemvtex%nsection), nelem(it), elemtype_gmshelem(it))
    n = 0
    do ic = 1, defgmsh%nelem
      if (ielem(ic) == it) then
        n = n+1
        elemvtex%elem(elemvtex%nsection)%ielem(n)  = ielem(ic)
        call set_elem(null(1), elemvtex%elem(elemvtex%nsection)%elemvtex(n,:), it, elem(ic,:))
      endif 
    enddo
    elemvtex%elem(elemvtex%nsection)%nelem = n
  endif
enddo

! --- create marks ---

maxtag = maxval(itag(1,:))
do it = 1, maxtag
enddo

! --- end ---

deallocate(ielem, itype, itag, elem, nelem)

call gmshseek_section(defgmsh, '$EndElements')

endsubroutine gmshread_elemvtex

!------------------------------------------------------------------------------!
! Function : compute number of VTEX in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function nvtex_gmshelement(itype)
implicit none
! -- dummy arguments --
integer(kpp), intent(in)  :: itype

select case(itype)
case(gmsh_node1)
  nvtex_gmshelement = 1
case(gmsh_bar2)
  nvtex_gmshelement = 2
case(gmsh_bar3)
  nvtex_gmshelement = 3
case(gmsh_bar4)
  nvtex_gmshelement = 4
case(gmsh_bar5)
  nvtex_gmshelement = 5
case(gmsh_tri3)
  nvtex_gmshelement = 3
case(gmsh_tri6)
  nvtex_gmshelement = 6
case(gmsh_tri9)
  nvtex_gmshelement = 9
case(gmsh_tri10)
  nvtex_gmshelement = 10
case(gmsh_tri12)
  nvtex_gmshelement = 12
case(gmsh_tri15)
  nvtex_gmshelement = 15
case(gmsh_quad4)
  nvtex_gmshelement = 4
case(gmsh_quad8)
  nvtex_gmshelement = 8
case(gmsh_quad9)
  nvtex_gmshelement = 9
case(gmsh_tetra4)
  nvtex_gmshelement = 4
case(gmsh_tetra10)
  nvtex_gmshelement = 10
case(gmsh_tetra20)
  nvtex_gmshelement = 20
case(gmsh_tetra35)
  nvtex_gmshelement = 35
case(gmsh_hexa8)
  nvtex_gmshelement = 8
case(gmsh_hexa20)
  nvtex_gmshelement = 20
case(gmsh_hexa27)
  nvtex_gmshelement = 27
case(gmsh_hexa64)
  nvtex_gmshelement = 64
case(gmsh_hexa125)
  nvtex_gmshelement = 125
case(gmsh_penta6)
  nvtex_gmshelement = 6
case(gmsh_penta15)
  nvtex_gmshelement = 18
case(gmsh_penta18)
  nvtex_gmshelement = 13
case(gmsh_pyra5)
  nvtex_gmshelement = 5
case(gmsh_pyra14)
  nvtex_gmshelement = 14
case(gmsh_pyra13)
  nvtex_gmshelement = 13
case default
  nvtex_gmshelement = -1
endselect

endfunction nvtex_gmshelement

!------------------------------------------------------------------------------!
! Function : equivalent typhon type of GMSH element
!------------------------------------------------------------------------------!
integer(kpp) function elemtype_gmshelem(gtype)
implicit none
! -- dummy arguments --
integer(kpp), intent(in)  :: gtype
integer(kpp)              :: itype
integer(kip), dimension(maxnbnodes) :: elem1, elem2

call set_elem_gmshelem(itype, elem1, gtype, elem2)  ! dummy elems
elemtype_gmshelem = itype

endfunction elemtype_gmshelem

!------------------------------------------------------------------------------!
! Function : transpose GMSH element vtex connectivity to internal element
!------------------------------------------------------------------------------!
subroutine set_elem_gmshelem(itype, elem, gtype, gmshelem)
implicit none
! -- dummy arguments --
integer(kpp), intent(in)  :: gtype
integer(kip), intent(in)  :: gmshelem(:)
integer(kpp), intent(out) :: itype
integer(kip), intent(out) :: elem(:)

select case(gtype)
case(gmsh_node1)
  itype   = elem_node
  elem(1) = gmshelem(1)
case(gmsh_bar2)
  itype     = elem_bar2
  elem(1:2) = gmshelem(1:2)
case(gmsh_bar3)
  itype     = elem_bar3
  elem(1:3) = gmshelem(1:3)
case(gmsh_bar4, gmsh_bar5)
  call cfd_error("GMSH: unexpected element BARn")
case(gmsh_tri3)
  itype     = elem_tri3
  elem(1:3) = gmshelem(1:3)
case(gmsh_tri6)
  itype     = elem_tri6
  elem(1:6) = gmshelem(1:6)
case(gmsh_tri9, gmsh_tri10, gmsh_tri12, gmsh_tri15)
  call cfd_error("GMSH: unexpected element TRIn")
case(gmsh_quad4)
  itype     = elem_quad4
  elem(1:4) = gmshelem(1:4)
case(gmsh_quad8)
  itype     = elem_quad8
  elem(1:8) = gmshelem(1:8)
case(gmsh_quad9)
  itype     = elem_quad9
  elem(1:9) = gmshelem(1:9)
case(gmsh_tetra4)
  itype     = elem_tetra4
  elem(1:4) = gmshelem(1:4)
case(gmsh_hexa8)
  itype     = elem_hexa8
  elem(1:8) = gmshelem(1:8)
case default
  call cfd_error("GMSH: unknown element")
endselect

endsubroutine set_elem_gmshelem



endmodule
!------------------------------------------------------------------------------!
! history:
! Mar  2014: creation
!------------------------------------------------------------------------------!

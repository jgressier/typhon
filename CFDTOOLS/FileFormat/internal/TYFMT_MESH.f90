!------------------------------------------------------------------------------!
! MODULE : TYFMT_MESH
!
!------------------------------------------------------------------------------!

module TYFMT_MESH

use XBIN_DATA
use TYPHON_FMT
use MESHPREC         ! Precision configuration
use USTMESH

implicit none

! -- Global Variables -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! ST_DEFTYPHON
!------------------------------------------------------------------------------!
type st_deftymesh
  integer(xbinkip) :: ncell, nface, nvtex
  integer(xbinkip) :: ncellsections
  integer(xbinkip) :: nfacesections
  integer(xbinkip) :: nmark
endtype st_deftymesh

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! write 
!------------------------------------------------------------------------------!
subroutine typhonwrite_zonemesh(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_xbindatasection) :: xbindata
! -- OUTPUTS --
! -- private data --
integer                       :: info
integer(xbinkip)              :: ngrid

! -- write ZONE info -- 

ngrid = 1

! File definition

call xbin_defdatasection(xbindata, xbinty_filedef, "MESH", &
     (/ xty_defaultver, &    ! TYPHON internal format version
        xty_file_mesh,  &    ! this file is a MESH file ".tyg"
        ngrid           &    ! number of grids
      /) )
call xbin_writedata_nodata(defxbin, xbindata)

endsubroutine typhonwrite_zonemesh


!------------------------------------------------------------------------------!
! write ELEMVTEX
!------------------------------------------------------------------------------!
subroutine typhonwrite_elemvtex(defxbin, elemvtex)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_elemvtex)        :: elemvtex
! -- OUTPUTS --
! -- private data --
integer :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkip), allocatable :: icell(:,:)
integer(xbinkip)              :: nelem, nvtex

!------------------------------------------------------------------------------!
! define ELEMENT header

call xbin_defdatasection(xbindata, xbinty_cells, "CELLS", &
     (/ elemvtex%elemtype &    ! type of element (see ELEMVTEX module)
      /) )

!------------------------------------------------------------------------------!
! write CELL elements

nelem  = elemvtex%nelem
nvtex  = elemvtex%nvtex
allocate(icell(1:nvtex, 1:nelem))
icell(1:nvtex, 1:nelem) = transpose(elemvtex%elemvtex(1:nelem,1:nvtex))

call xbin_writedata_indint(defxbin, xbindata, nelem, elemvtex%ielem, nvtex, icell)
call delete_xbindata(xbindata)
deallocate(icell)

endsubroutine typhonwrite_elemvtex

!------------------------------------------------------------------------------!
! read ELEMVTEX
!------------------------------------------------------------------------------!
subroutine typhonread_elemvtex(defxbin, xbintype, elemvtex)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
integer(xbinkpp)         :: xbintype
! -- OUTPUTS --
type(st_elemvtex)        :: elemvtex
! -- private data --
integer :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkip), allocatable :: icell(:,:)
integer(xbinkip)              :: nelem, nvtex

!------------------------------------------------------------------------------!
! check ELEMENT header

call xbin_readdatahead(defxbin, xbindata)

if (xbindata%usertype /= xbintype) then
  select case(xbintype)
  case(xbinty_cells)
    call cfd_error("XBIN/TYPHON error: expecting CELL data section")
  case(xbinty_faces)
    call cfd_error("XBIN/TYPHON error: expecting FACE data section")
  case default
    call cfd_error("XBIN/TYPHON error: expecting !unknown! data section")
  endselect
endif

if (xbindata%nparam >= 1) then
  elemvtex%elemtype = xbindata%param(1)
else
  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
endif

!------------------------------------------------------------------------------!
! read CELL elements

nelem  = xbindata%nelem
nvtex  = xbindata%dim
allocate(elemvtex%ielem(1:nelem))
allocate(icell(1:nvtex, 1:nelem))

call xbin_readdata_indint(defxbin, xbindata, elemvtex%ielem, icell)

allocate(elemvtex%elemvtex(1:nelem,1:nvtex))
elemvtex%elemvtex(1:nelem,1:nvtex) = transpose(icell(1:nvtex, 1:nelem))
elemvtex%nelem    = nelem
elemvtex%nvtex    = nvtex
if (nvtex_element(elemvtex%elemtype) /= elemvtex%nvtex) then
  call cfd_error("non-matching number of vertices in XBIN CELL section")
endif

call delete_xbindata(xbindata)
deallocate(icell)

endsubroutine typhonread_elemvtex


!------------------------------------------------------------------------------!
! write NODES coordinates of a mesh
!------------------------------------------------------------------------------!
subroutine typhonwrite_nodes(defxbin, mesh)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_mesh)            :: mesh
! -- OUTPUTS --
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
real(xbinkrp), allocatable :: vtex(:,:)
integer(xbinkip)           :: nelem, dim, i

!------------------------------------------------------------------------------!
! define  header

call xbin_defdatasection(xbindata, xbinty_nodes, "NODES")

!------------------------------------------------------------------------------!
! write NODES coordinates

nelem = mesh%nvtex
dim   = 3
allocate(vtex(1:3,1:nelem))

do i = 1, nelem
  vtex(1,i) = mesh%vertex(i,1,1)%x
  vtex(2,i) = mesh%vertex(i,1,1)%y
  vtex(3,i) = mesh%vertex(i,1,1)%z
enddo

call xbin_writedata_ordreal(defxbin, xbindata, nelem, dim, vtex)
call delete_xbindata(xbindata)
deallocate(vtex)

endsubroutine typhonwrite_nodes

!------------------------------------------------------------------------------!
! read NODES coordinates of a mesh
!------------------------------------------------------------------------------!
subroutine typhonread_nodes(defxbin, mesh)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
! -- OUTPUTS --
type(st_mesh)            :: mesh
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
real(xbinkrp), allocatable :: vtex(:,:)
integer(xbinkip)           :: nelem, dim, i

!------------------------------------------------------------------------------!
! check header

call xbin_readdatahead(defxbin, xbindata)

if ((xbindata%name /= "NODES").or.(xbindata%usertype /= xbinty_nodes)) then
  call cfd_error("XBIN/TYPHON error: expecting NODE data section")
endif

!if (xbindata%nparam >= 1) then
!  elemvtex%elemtype = xbindata%param(1)
!else
!  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
!endif

!------------------------------------------------------------------------------!
! read NODES coordinates

allocate(vtex(1:xbindata%dim,1:xbindata%nelem))

call xbin_readdata_ordreal(defxbin, xbindata, vtex)

mesh%nvtex = xbindata%nelem
allocate(mesh%vertex(xbindata%nelem, 1, 1))

do i = 1, xbindata%nelem
  mesh%vertex(i,1,1)%x = vtex(1,i) 
  mesh%vertex(i,1,1)%y = vtex(2,i) 
  mesh%vertex(i,1,1)%z = vtex(3,i) 
enddo

call delete_xbindata(xbindata)
deallocate(vtex)

endsubroutine typhonread_nodes

!------------------------------------------------------------------------------!
! write BC FACES 
!------------------------------------------------------------------------------!
subroutine typhonwrite_bcfaces_bcmarks(defxbin, umesh) 
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_ustmesh)         :: umesh
! -- OUTPUTS --
! -- private data --
type(st_xbindatasection)      :: xbindata
integer(xbinkip)              :: nelem, dim
integer(xbinkip)              :: nelemtot, nvtexmax, nv, nf
integer(xbinkip)              :: i, j, ib, iff, isec, ielem, istart, iend
integer(xbinkip)              :: elemtype, ibc, nface
integer(xbinkip), allocatable :: iface(:), nbelem(:), newindex(:), elem(:,:)

!------------------------------------------------------------------------------!
! sort faces by element type

nelemtot = umesh%nface_lim

! -- count number of elements per type --

nvtexmax = umesh%facevtex%nbfils
allocate(nbelem(nvtexmax))          ! number of boco face 
nbelem(:) = 0

allocate(newindex(1:nelemtot))

do i = 1, nelemtot
  nv = count(umesh%facevtex%fils(umesh%nface_int+i, 1:nvtexmax) /= 0)
  nbelem(nv)   = nbelem(nv)+1
enddo

!------------------------------------------------------------------------------!
! collect and write element section

iend = 0

do isec = 1, nvtexmax

  if (nbelem(isec) >= 1) then

    select case(isec)
    case(2)
       elemtype = elem_bar2
    case(3)
       elemtype = elem_tri3
    case(4)
       elemtype = elem_quad4
    case default
       call cfd_error("Fatal error writing TYPHON boundaries: unknown element type")
    endselect

    allocate(elem(1:isec, 1:nbelem(isec)))

    iff = 0
    do i = 1, nelemtot
      if (count(umesh%facevtex%fils(umesh%nface_int+i, 1:nvtexmax) /= 0) == isec) then
        iff = iff +1
        elem(1:isec, iff) = umesh%facevtex%fils(umesh%nface_int+i, 1:isec)
        newindex(i)       = iend + iff
      endif
    enddo

    istart = iend   +1
    iend   = istart -1 + nbelem(isec)

    ! -- write FACE section --

    call xbin_defdatasection(xbindata, xbinty_faces, "FACES", &
       (/ elemtype  &    ! type of element (see ELEMVTEX module)
        /) )

    call xbin_writedata_ordint(defxbin, xbindata, nbelem(isec), isec, elem, firstindex=istart)
    call delete_xbindata(xbindata)

    deallocate(elem)
  endif
enddo

!------------------------------------------------------------------------------!
! write BOCO marks

do ib = 1, umesh%nboco

  nface = umesh%boco(ib)%nface
  allocate(iface(1:nface))
  iface(1:nface) = newindex(umesh%boco(ib)%iface(1:nface)-umesh%nface_int)

  ! -- write FACE section --

  call xbin_defdatasection(xbindata, xbinty_marks, trim(umesh%boco(ib)%family))

  call xbin_writedata_ordint(defxbin, xbindata, umesh%boco(ib)%nface, iface)
  call delete_xbindata(xbindata)

  deallocate(iface)

enddo

endsubroutine typhonwrite_bcfaces_bcmarks

!------------------------------------------------------------------------------!
! read BC FACES marks
!------------------------------------------------------------------------------!
subroutine typhonread_bcmark(defxbin, boco) 
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
! -- OUTPUTS --
type(st_ustboco)         :: boco
! -- private data --
type(st_xbindatasection) :: xbindata

!------------------------------------------------------------------------------!
! read BOCO mark

call xbin_readdatahead(defxbin, xbindata)

if (xbindata%usertype /= xbinty_marks) then
  call cfd_error("XBIN/TYPHON error: expecting BC MARK data section")
endif

!if (xbindata%nparam >= 1) then
!  elemvtex%elemtype = xbindata%param(1)
!else
!  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
!endif

call new_ustboco(boco, trim(xbindata%name), 0)
boco%ilocation = iloc_elemface
boco%ntag      = xbindata%nelem
allocate(boco%itag(1:boco%ntag))

call xbin_readdata_ordint(defxbin, xbindata, boco%itag(1:boco%ntag))
call delete_xbindata(xbindata)

endsubroutine typhonread_bcmark


endmodule TYFMT_MESH
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2011: split from TYPHON_FMT 
!------------------------------------------------------------------------------!

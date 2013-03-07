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
  integer(xbinkip)           :: ncell, nface, nvtex
  integer(xbinkip)           :: ncellsections, nfacesections
  integer(xbinkip)           :: ncellmark,     nfacemark
  character(len=xbin_strlen) :: sharedmesh
endtype st_deftymesh

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Write a USTMESH structure to XBIN format
!------------------------------------------------------------------------------!
subroutine typhonwrite_ustmesh(deftyphon, umesh, meshfilename)
implicit none
! -- INPUTS --
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
character(len=*), optional :: meshfilename
! -- OUTPUTS --
! -- private data --
integer :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkip)              :: ielem

! -- BODY --

!------------------------------------------------------------------------------!
! write GRID header

select case(deftyphon%meshdef)
case(mesh_full)
  call typhonwrite_umeshheader(deftyphon, umesh)
case(mesh_shared, mesh_sharedcon)
  if (present(meshfilename)) then
    call typhonwrite_umeshheader(deftyphon, umesh, meshfilename)
  else
    call cfd_error("TYPHON format IO: missing shared mesh filename")
  endif
case default
  call cfd_error("TYPHON format IO: unknown mesh definition")
endselect

!------------------------------------------------------------------------------!
! write CELL elements (if mesh_full only)

if (deftyphon%meshdef == mesh_full) then
  do ielem = 1, umesh%cellvtex%nsection
    call typhonwrite_elemvtex(deftyphon%defxbin, umesh%cellvtex%elem(ielem))
  enddo
endif

!------------------------------------------------------------------------------!
! write NODES coordinates (if mesh_full or mesh_sharedcon)

if (deftyphon%meshdef /= mesh_shared) then
  call typhonwrite_nodes(deftyphon%defxbin, umesh%mesh)
endif

!------------------------------------------------------------------------------!
! write BC FACES coordinates (if mesh_full only)

if (deftyphon%meshdef == mesh_full) then
  call typhonwrite_bcfaces_bcmarks(deftyphon%defxbin, umesh)
endif

endsubroutine typhonwrite_ustmesh

!------------------------------------------------------------------------------!
! write USTMESH header
!------------------------------------------------------------------------------!
subroutine typhonwrite_umeshheader(deftyphon, umesh, meshfilename)
implicit none
! -- INPUTS --
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
character(len=*), optional :: meshfilename
! -- OUTPUTS --
! -- private data --
integer                       :: info
type(st_xbindatasection)      :: xbindata
character(len=xbin_strlen)    :: filename
integer(xbinkip)              :: ncell, nface, nvtex, nfacemark, ncellmark
integer(xbinkip)              :: ncellsections, nfacesections

! -- BODY --

ncell         = umesh%ncell_int
nface         = umesh%nface_lim
nvtex         = umesh%nvtex
nfacemark     = umesh%nboco
ncellmark     = 0
ncellsections = umesh%cellvtex%nsection
nfacesections = 1

if (present(meshfilename)) then
  filename = meshfilename
else
  filename = ""
endif

!------------------------------------------------------------------------------!
! write GRID header

call xbin_defdatasection(xbindata, xbinty_meshdef, "USTMESH", &
      (/ xty_grid_umesh, &    ! GRID type
         ncell,          &    ! number of interior cells
         nface,          &    ! number of faces (boco, marked, ...)
         nvtex,          &    ! number of number of vertices
         ncellsections,  &    ! number of cell sections
         nfacesections,  &    ! number of face sections
         ncellmark,      &    ! number of mark sections
         nfacemark       &    ! number of mark sections
       /), trim(filename) )
call xbin_writedata_nodata(deftyphon%defxbin, xbindata)
call delete_xbindata(xbindata)

endsubroutine typhonwrite_umeshheader

!------------------------------------------------------------------------------!
! read USTMESH header
!------------------------------------------------------------------------------!
subroutine typhonread_umeshheader(deftyphon, umesh, deftymesh)
implicit none
! -- INPUTS --
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
! -- OUTPUTS --
type(st_deftymesh)       :: deftymesh
! -- private data --
integer :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkpp)              :: nparam
integer(xbinkip), allocatable :: param(:)
integer(xbinkip)              :: ncell, nface, nvtex, ncellmark, nfacemark, ielem, ib
integer(xbinkip)              :: ncellsections, nfacesections

! -- BODY --

!------------------------------------------------------------------------------!
! read GRID header

call xbin_readdatahead(deftyphon%defxbin, xbindata)
call xbin_skipdata(deftyphon%defxbin, xbindata)

select case(deftyphon%xty_version)
case(1)
  if (xbindata%usertype /= xbinty_filedef) call cfd_error("XBIN/TYPHON error: expecting USTMESH data section")
case(2:xty_maxver)
  if (xbindata%usertype /= xbinty_meshdef) call cfd_error("XBIN/TYPHON error: expecting USTMESH data section")
case default
  call cfd_error("XBIN/TYPHON error: unexpected version number")
endselect

select case (deftyphon%xty_version)
case(1)
  if (xbindata%nparam /= 7) call cfd_error("XBIN/TYPHON error: bad number parameters in USTMESH header")
  deftymesh%ncell         = xbindata%param(2)
  deftymesh%nface         = xbindata%param(3)
  deftymesh%nvtex         = xbindata%param(4)
  deftymesh%ncellsections = xbindata%param(5)
  deftymesh%nfacesections = xbindata%param(6)
  deftymesh%ncellmark     = 0
  deftymesh%nfacemark     = xbindata%param(7)
case(2:xty_maxver)
  if (xbindata%nparam /= 8) call cfd_error("XBIN/TYPHON error: bad number parameters in USTMESH header")
  deftymesh%ncell         = xbindata%param(2)
  deftymesh%nface         = xbindata%param(3)
  deftymesh%nvtex         = xbindata%param(4)
  deftymesh%ncellsections = xbindata%param(5)
  deftymesh%nfacesections = xbindata%param(6)
  deftymesh%ncellmark     = xbindata%param(7)
  deftymesh%nfacemark     = xbindata%param(8)
case default
  call cfd_error("XBIN/TYPHON error: unexpected version number")
endselect

deftymesh%sharedmesh = xbindata%string

call delete_xbindata(xbindata)

endsubroutine typhonread_umeshheader

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
integer(xbinkip)              :: nelem, nvtex, ic

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
do ic = 1, nelem
  icell(1:nvtex, ic) = elemvtex%elemvtex(ic,1:nvtex) 
enddo

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
integer                       :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkip), allocatable :: icell(:,:)
integer(xbinkip)              :: nelem, nvtex, ic

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
do ic = 1, nelem
  elemvtex%elemvtex(ic,1:nvtex) = icell(1:nvtex, ic)
enddo

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

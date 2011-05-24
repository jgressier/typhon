!------------------------------------------------------------------------------!
! Routine: typhonread_ustmesh
!
! Read a USTMESH structure to XBIN format
!------------------------------------------------------------------------------!
subroutine typhonread_ustmesh(deftyphon, umesh)

use IO_UNIT
use XBIN_IO
use XBIN_DATA
use TYPHON_FMT
use TYFMT_MESH
use USTMESH

implicit none

! -- INPUTS --
type(st_deftyphon)       :: deftyphon
type(st_ustmesh)         :: umesh

! -- OUTPUTS --

! -- private data --
integer                       :: info, iunit
type(st_deftyphon)            :: deftyphon2
type(st_xbindatasection)      :: xbindata
integer(xbinkpp)              :: nparam
integer(xbinkip), allocatable :: param(:)
type(st_deftymesh)            :: deftymesh, deftymesh2
integer(xbinkip)              :: ielem, ib

! -- BODY --

call init_ustmesh(umesh, 1)   ! default values initialization

! -- dimensions --

!------------------------------------------------------------------------------!
! read GRID header

call cfd_print("> mesh header")

call typhonread_umeshheader(deftyphon, umesh, deftymesh)

! -- if shared mesh --

select case(deftyphon%meshdef)
case(mesh_full) 
  deftyphon2 = deftyphon
  deftymesh2 = deftymesh
case(mesh_shared, mesh_sharedcon)
  if (deftymesh%sharedmesh /= "") then
    iunit = getnew_io_unit()
    call cfd_print("> open shared mesh "//trim(deftymesh%sharedmesh))
    call typhon_openread(iunit, trim(deftymesh%sharedmesh), deftyphon2)
    call typhonread_umeshheader(deftyphon2, umesh, deftymesh2)
    if (deftyphon2%meshdef /= mesh_full) call cfd_error("TYPHON format: shared mesh is shared again")
  else
    call cfd_error("TYPHON format: empty shared mesh filename")
  endif
case default
  call cfd_error("TYPHON format: unknown mesh definition (typhonread_ustmesh)")
endselect

!------------------------------------------------------------------------------!
! read CELL elements

call cfd_print("> cell elements")

do ielem = 1, deftymesh2%ncellsections
  call addelem_genelemvtex(umesh%cellvtex)
  call typhonread_elemvtex(deftyphon2%defxbin, xbinty_cells, umesh%cellvtex%elem(umesh%cellvtex%nsection))
enddo

!------------------------------------------------------------------------------!
! read NODES coordinates

call cfd_print("> nodes coordinates")

call typhonread_nodes(deftyphon2%defxbin, umesh%mesh)

! -- if only shared connectivity, read again nodes in original mesh --

if (deftyphon%meshdef == mesh_sharedcon) call typhonread_nodes(deftyphon%defxbin, umesh%mesh)

umesh%nvtex = umesh%mesh%nvtex

!------------------------------------------------------------------------------!
! read BC FACES connectivity

call cfd_print("> boundary faces")

do ielem = 1, deftymesh2%nfacesections
  call addelem_genelemvtex(umesh%cellvtex)
  call typhonread_elemvtex(deftyphon2%defxbin, xbinty_faces, umesh%cellvtex%elem(umesh%cellvtex%nsection))
enddo

!------------------------------------------------------------------------------!
! read BC FACES marks

call cfd_print("> boundary marks")

call createboco(umesh, deftymesh2%nfacemark)

do ib = 1, deftymesh2%nfacemark
  call typhonread_bcmark(deftyphon2%defxbin, umesh%boco(ib))
enddo

!------------------------------------------------------------------------------!
if (deftyphon%meshdef /= mesh_full) call close_io_unit(iunit)

call check_ustmesh_elements(umesh)

endsubroutine typhonread_ustmesh
!------------------------------------------------------------------------------!
! Change history
! Apr 2011: created
! May 2011: able to read shared mesh
!------------------------------------------------------------------------------!

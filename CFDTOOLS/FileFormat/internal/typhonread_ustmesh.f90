!------------------------------------------------------------------------------!
! Routine: typhonread_ustmesh
!
! Read a USTMESH structure to XBIN format
!------------------------------------------------------------------------------!
subroutine typhonread_ustmesh(defxbin, umesh)

use XBIN_IO
use XBIN_DATA
use TYPHON_FMT
use USTMESH

implicit none

! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_ustmesh)         :: umesh

! -- OUTPUTS --

! -- private data --
integer :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkpp)              :: nparam
integer(xbinkip), allocatable :: param(:)
integer(xbinkip)              :: ncell, nface, nvtex, nmark, ielem, ib
integer(xbinkip)              :: ncellsections, nfacesections

! -- BODY --

call init_ustmesh(umesh)   ! default values initialization

! -- dimensions --

ncell         = umesh%ncell_int
nface         = umesh%nface_lim
nvtex         = umesh%nvtex
nmark         = umesh%nboco
ncellsections = umesh%cellvtex%nsection
nfacesections = 1

!------------------------------------------------------------------------------!
! read GRID header

call cfd_print("> header")

call xbin_readdatahead(defxbin, xbindata)
call xbin_skipdata(defxbin, xbindata)

!call xbin_defdatasection(xbindata, xbinty_filedef, "USTMESH", &
!      (/ xty_grid_umesh, &    ! GRID type
!         ncell,          &    ! number of interior cells
!         nface,          &    ! number of faces (boco, marked, ...)
!         nvtex,          &    ! number of number of vertices
!         ncellsections,  &    ! number of cell sections
!         nfacesections,  &    ! number of face sections
!         nmark           &    ! number of mark sections
!       /) )

if ((xbindata%name /= "USTMESH").or.(xbindata%usertype /= xbinty_filedef)) then
  call cfd_error("XBIN/TYPHON error: expecting USTMESH data section")
endif

if (xbindata%nparam >= 7) then
  ncell         = xbindata%param(2)
  nface         = xbindata%param(3)
  nvtex         = xbindata%param(4)
  ncellsections = xbindata%param(5)
  nfacesections = xbindata%param(6)
  nmark         = xbindata%param(7)
else
  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
endif

call delete_xbindata(xbindata)

!------------------------------------------------------------------------------!
! read CELL elements

call cfd_print("> cell elements")

do ielem = 1, ncellsections
  call addelem_genelemvtex(umesh%cellvtex)
  call typhonread_elemvtex(defxbin, xbinty_cells, umesh%cellvtex%elem(umesh%cellvtex%nsection))
enddo

!------------------------------------------------------------------------------!
! read NODES coordinates

call cfd_print("> nodes coordinates")

call typhonread_nodes(defxbin, umesh%mesh)
umesh%nvtex = umesh%mesh%nvtex

!------------------------------------------------------------------------------!
! read BC FACES connectivity

call cfd_print("> boundary faces")

do ielem = 1, nfacesections
  call addelem_genelemvtex(umesh%cellvtex)
  call typhonread_elemvtex(defxbin, xbinty_faces, umesh%cellvtex%elem(umesh%cellvtex%nsection))
enddo

!------------------------------------------------------------------------------!
! read BC FACES marks

call cfd_print("> boundary marks")

call createboco(umesh, nmark)

do ib = 1, nmark
  call typhonread_bcmark(defxbin, umesh%boco(ib))
enddo

endsubroutine typhonread_ustmesh
!------------------------------------------------------------------------------!
! Change history
!
!------------------------------------------------------------------------------!

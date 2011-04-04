!------------------------------------------------------------------------------!
! Routine: typhonwrite_ustmesh
!
! Write a USTMESH structure to XBIN format
!------------------------------------------------------------------------------!
subroutine typhonwrite_ustmesh(defxbin, umesh)

use XBIN_IO
use XBIN_DATA
use TYFMT_MESH
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
integer(xbinkip)              :: ncell, nface, nvtex, nmark, ielem
integer(xbinkip)              :: ncellsections, nfacesections

! -- dimensions --

ncell         = umesh%ncell_int
nface         = umesh%nface_lim
nvtex         = umesh%nvtex
nmark         = umesh%nboco
ncellsections = umesh%cellvtex%nsection
nfacesections = 1

!------------------------------------------------------------------------------!
! write GRID header

call xbin_defdatasection(xbindata, xbinty_filedef, "USTMESH", &
      (/ xty_grid_umesh, &    ! GRID type
         ncell,          &    ! number of interior cells
         nface,          &    ! number of faces (boco, marked, ...)
         nvtex,          &    ! number of number of vertices
         ncellsections,  &    ! number of cell sections
         nfacesections,  &    ! number of face sections
         nmark           &    ! number of mark sections
       /) )
call xbin_writedata_nodata(defxbin, xbindata)
call delete_xbindata(xbindata)

!------------------------------------------------------------------------------!
! write CELL elements

do ielem = 1, umesh%cellvtex%nsection
  call typhonwrite_elemvtex(defxbin, umesh%cellvtex%elem(ielem))
enddo

!------------------------------------------------------------------------------!
! write NODES coordinates

call typhonwrite_nodes(defxbin, umesh%mesh)

!------------------------------------------------------------------------------!
! write BC FACES coordinates

call typhonwrite_bcfaces_bcmarks(defxbin, umesh)

endsubroutine typhonwrite_ustmesh
!------------------------------------------------------------------------------!
! Changes
!
! June 2010: creation
!------------------------------------------------------------------------------!

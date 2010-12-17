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
integer(xbinkip)              :: ncell, nface, nvtex, nmark, ielem
integer(xbinkip)              :: ncellsections, nfacesections

! -- BODY --

call init_ustmesh(umesh)   ! default values initialization

! -- dimensions --

ncell         = umesh%ncell_int
nface         = umesh%nface_lim
nvtex         = umesh%nvtex
nmark         = umesh%nboco
ncellsections = umesh%cellvtex%ntype
nfacesections = 1

!------------------------------------------------------------------------------!
! read GRID header

call xbin_readdatahead(defxbin, xbindata)
call xbin_skipdata(defxbin, xbindata)

if (xbindata%name /= "USTMESH") &
  call cfd_error("unexpected data section when reading USTMESH")



call xbin_defdatasection(xbindata, xbinty_filedef, "USTMESH", &
      (/ xty_grid_umesh, &    ! GRID type
         ncell,          &    ! number of interior cells
         nface,          &    ! number of faces (boco, marked, ...)
         nvtex,          &    ! number of number of vertices
         ncellsections,  &    ! number of cell sections
         nfacesections,  &    ! number of face sections
         nmark           &    ! number of mark sections
       /) )
call xbin_readdata_nodata(defxbin, xbindata)
call delete_xbindata(xbindata)

!------------------------------------------------------------------------------!
! read CELL elements

do ielem = 1, umesh%cellvtex%ntype
  call typhonread_elemvtex(defxbin, umesh%cellvtex%elem(ielem))
enddo

!------------------------------------------------------------------------------!
! read NODES coordinates

call typhonread_nodes(defxbin, umesh%mesh)

!------------------------------------------------------------------------------!
! read BC FACES coordinates

call typhonread_bcfaces_bcmarks(defxbin, umesh)

endsubroutine typhonread_ustmesh
!------------------------------------------------------------------------------!
! Change history
!
!------------------------------------------------------------------------------!

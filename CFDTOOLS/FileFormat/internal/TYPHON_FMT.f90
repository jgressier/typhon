!------------------------------------------------------------------------------!
! MODULE : TYPHON_FMT
!
!------------------------------------------------------------------------------!

module TYPHON_FMT

use XBIN_DATA
use MESHPREC         ! Precision configuration
use USTMESH

implicit none

! -- Global Variables -------------------------------------------

integer(xbinkip),  parameter :: xty_defaultver  = 1
integer(xbinkip),  parameter :: xty_maxver      = 1

character(len=3),  parameter :: xtymeshext = "tyg"
character(len=3),  parameter :: xtysolext  = "tys"
character(len=3),  parameter :: xtymonext  = "tym"

! -- DECLARATIONS -----------------------------------------------------------

! -- XBIN DATA section TYPE --

integer(xbinkpp), parameter :: xbinty_filedef = 1
integer(xbinkpp), parameter :: xbinty_meshdef = 10
integer(xbinkpp), parameter :: xbinty_nodes   = 11
integer(xbinkpp), parameter :: xbinty_cells   = 12
integer(xbinkpp), parameter :: xbinty_faces   = 15
integer(xbinkpp), parameter :: xbinty_marks   = 20

! -- Parameters --

integer(xbinkip), parameter :: xty_file_mesh    = 10
integer(xbinkip), parameter :: xty_file_sol     = 20
integer(xbinkip), parameter :: xty_file_monitor = 50

integer(xbinkip), parameter :: xty_mesh_umesh     = 10    ! mono-grid   unstructured mesh
integer(xbinkip), parameter :: xty_mesh_partumesh = 15    ! partitioned unstructured mesh

integer(xbinkip), parameter :: xty_grid_umesh     = 10    ! mono-grid   unstructured mesh
integer(xbinkip), parameter :: xty_grid_partumesh = 15    ! partitioned unstructured mesh

!------------------------------------------------------------------------------!
! ST_TYPHONPROJECT
!------------------------------------------------------------------------------!
type st_internalproject
  integer(xbinkpp) :: version_prj, version_mesh, version_sol      ! version numbers of files
endtype st_internalproject

!------------------------------------------------------------------------------!
! ST_TYPHONMESH
!------------------------------------------------------------------------------!
type st_internalmesh
  integer :: unit
endtype st_internalmesh


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

endsubroutine typhonwrite_elemvtex


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

call xbin_defdatasection(xbindata, xbinty_cells, "NODES")

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

endsubroutine typhonwrite_nodes

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
integer(xbinkrp), allocatable :: vtex(:,:)
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



endmodule TYPHON_FMT
!------------------------------------------------------------------------------!
! Changes
!
! June 2010: 
!------------------------------------------------------------------------------!

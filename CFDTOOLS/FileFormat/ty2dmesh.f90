!------------------------------------------------------------------------------!
! TY2DMESH
! creates an H structured mesh 
!
!------------------------------------------------------------------------------!
program ty2dmesh

use IO_UNIT
use USTMESH
use XBIN_IO
use TYPHON_FMT

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs, iunit
character(len=160) :: filename
type(st_defxbin)   :: defxbin
integer(kip)       :: nx, ny
real(krp)          :: lx, ly
type(st_ustmesh)   :: umesh
integer(kpp)       :: itype, ielem
!---------------------------
integer(kip)       :: i, j, iv, ic, nelem, nvtex
!------------------------------------------------------------------------------!

print*,'--- TY2DMESH ---'

!------------------------------
! parse arguments

nargs = command_argument_count()

if (nargs /= 1) then
  call cfd_error("bad number of command line arguments")
else
  call get_command_argument(1, filename)
endif

!------------------------------------------------------------
! default: creates a 100x100 uniform mesh in 1x1 box
!------------------------------------------------------------

lx = 1._krp
ly = 1._krp
nx = 100
ny = 100

print*,'creating ',nx,'x',ny,' 2D mesh'
call init_ustmesh(umesh)

!------------------------------
! creates VERTICES
!------------------------------
print*,'. vertices'
umesh%mesh%nvtex  = (nx+1)*(ny+1)
umesh%nvtex       = umesh%mesh%nvtex                   ! nb of vertices (redundant)

allocate(umesh%mesh%vertex(1:umesh%mesh%nvtex, 1, 1))
do i = 1, nx+1
  do j = 1, ny+1
    iv = (i-1)*(ny+1)+j
    umesh%mesh%vertex(iv, 1, 1) = v3d( (i-1)*(lx/nx), (j-1)*(ly/ny), 0._krp )
  enddo
enddo

!------------------------------
! creates QUAD elements (iso-i lines)
!------------------------------
print*,'. QUAD elements'
itype = elem_quad4

ielem = getindex_genelemvtex(umesh%cellvtex, itype)

if (ielem /= 0) call cfd_error("element section already exists")

nelem = nx*ny

call addelem_genelemvtex(umesh%cellvtex)
ielem = umesh%cellvtex%nsection
call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, itype)
nvtex = umesh%cellvtex%elem(ielem)%nvtex

do i = 1, nx
  do j = 1, ny
    ic = (i-1)*ny+j         ! cell index
    iv = (i-1)*(ny+1)+j     ! bottom left corner
    umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:nvtex) = (/ iv, iv+(ny+1), iv+(ny+1)+1, iv+1 /)
    umesh%cellvtex%elem(ielem)%ielem   (ic)          = ic
  enddo
enddo

!------------------------------
! creates FACES elements (ONLY boundaries)
!------------------------------
print*,'. BC faces'

nelem = 2*(nx+ny)
call new(umesh%facevtex, nelem, 2)
umesh%nface     = nelem
umesh%nface_lim = nelem

do j = 1, ny
  umesh%facevtex%fils(   j, 1:2) = (/ j, j+1 /)         ! IMIN faces
  iv = (i-1)*(ny+1)                                ! bottom right corner
  umesh%facevtex%fils(ny+j, 1:2) = (/ iv+j, iv+j+1 /)   ! IMAX faces
enddo
do i = 1, nx
  umesh%facevtex%fils(2*ny   +i, 1:2) = (/ (i-1)*(ny+1)+1,     i*(ny+1)+1 /) ! JMIN faces
  umesh%facevtex%fils(2*ny+nx+i, 1:2) = (/     i*(ny+1),   (i+1)*(ny+1)   /) ! JMAX faces
enddo

!------------------------------
! BC marks
!------------------------------
print*,'. BC marks (IMIN, IMAX, JMIN, JMAX)'

call createboco(umesh, 4)       ! creates 4 boco (IMIN, IMAX, JMIN, JMAX)

call new_ustboco(umesh%boco(1), "IMIN", ny)
umesh%boco(1)%iface(1:ny) = (/ (j, j=1,ny) /)
call new_ustboco(umesh%boco(2), "IMAX", ny)
umesh%boco(2)%iface(1:ny) = (/ (ny+j, j=1,ny) /)
call new_ustboco(umesh%boco(3), "JMIN", nx)
umesh%boco(3)%iface(1:nx) = (/ (2*ny+i, i=1,nx) /)
call new_ustboco(umesh%boco(4), "JMAX", nx)
umesh%boco(4)%iface(1:nx) = (/ (2*ny+nx+i, i=1,nx) /)

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------
print*
print*,'opening file '//trim(filename)
!------------------------------
! open xbin file

iunit = getnew_io_unit()

call xbin_openwrite(iunit, trim(filename), defxbin)

call typhonwrite_ustmesh(defxbin, umesh)

!------------------------------
! close file and end program

call close_io_unit(iunit)
print*,'done.'

endprogram
!------------------------------------------------------------------------------!
! Changes
!
! May  2010: created, write cartesian 100x100 cells mesh of 1x1 box
!------------------------------------------------------------------------------!

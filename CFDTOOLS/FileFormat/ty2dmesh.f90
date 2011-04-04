!------------------------------------------------------------------------------!
! TY2DMESH
! creates an H structured mesh 
!
!------------------------------------------------------------------------------!
program ty2dmesh

use IO_UNIT
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs, iunit
character(len=160) :: filename, str_opt, str_val
type(st_deftyphon) :: deftyphon
integer(kip)       :: nx, ny
real(krp)          :: lx, ly
type(st_ustmesh)   :: umesh
integer(kpp)       :: itype, ielem, type_mesh
!---------------------------
integer(kip)       :: i, j, iv, ivc, ic, nelem, nvtex, iarg
!------------------------------------------------------------------------------!
integer(kpp), parameter :: mesh_quad = 1
integer(kpp), parameter :: mesh_tri  = 2
integer(kpp), parameter :: mesh_tri4 = 4

print*,'--- TY2DMESH ---'

!------------------------------
! parse arguments

! default values

lx = 1._krp
ly = 1._krp
nx = 100
ny = 100
filename  = ""
type_mesh = mesh_quad

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call get_command_argument(iarg, str_opt)
  iarg = iarg + 1
  if     (str_opt == "-nx")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) nx
  elseif (str_opt == "-ny")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) ny
  elseif (str_opt == "-lx")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) lx
  elseif (str_opt == "-ly")  then
    call get_command_argument(iarg, str_val)
    iarg = iarg + 1
    read(str_val, *) ly
  elseif (str_opt == "-quad") then
    type_mesh = mesh_quad
  elseif (str_opt == "-tri") then
    type_mesh = mesh_tri
  elseif (str_opt == "-tri4") then
    type_mesh = mesh_tri4
 else
    filename = trim(str_opt)
  endif
enddo

if (filename == "") then
  print*,"command line: ty2dmesh [options] filename.tym"
  print*,"available options:"
  print*,"  -nx 100   : number of I-cells"
  print*,"  -ny 100   : number of J-cells"
  print*,"  -lx 1     : domain length"
  print*,"  -ly 1     : domain height"
  print*,"  -quad     : generates quad"
  print*,"  -tri      : generates tri (split each quad)"
  print*,"  -tri4     : generates tri (split each quad into 4 tri)"
  call cfd_error("missing file name")
else
  ic = index(filename, ".tym")
  if (ic /= len(trim(filename))-3) filename = trim(filename)//".tym"
 endif

!------------------------------------------------------------
! default: creates a 100x100 uniform mesh in 1x1 box
!------------------------------------------------------------

select case(type_mesh)
case(mesh_quad)
  print*,'creating '//trim(strof(nx))//'x'//trim(strof(ny))//' 2D quad mesh'
case(mesh_tri)
  print*,'creating 2x'//trim(strof(nx))//'x'//trim(strof(ny))//' 2D tri mesh'
case(mesh_tri4)
  print*,'creating 4x'//trim(strof(nx))//'x'//trim(strof(ny))//' 2D tri mesh'
case default
  call cfd_error("unknown mesh type")
endselect

call init_ustmesh(umesh)

!------------------------------
! creates VERTICES
!------------------------------
print*,'. vertices'

select case(type_mesh)
case(mesh_quad, mesh_tri)
  umesh%mesh%nvtex  = (nx+1)*(ny+1)
case(mesh_tri4)
  umesh%mesh%nvtex  = (nx+1)*(ny+1)+nx*ny
case default
  call cfd_error("unknown mesh type")
endselect

umesh%nvtex       = umesh%mesh%nvtex                   ! nb of vertices (redundant)

allocate(umesh%mesh%vertex(1:umesh%mesh%nvtex, 1, 1))
do i = 1, nx+1
  do j = 1, ny+1
    iv = (i-1)*(ny+1)+j
    umesh%mesh%vertex(iv, 1, 1) = v3d( (i-1)*(lx/nx), (j-1)*(ly/ny), 0._krp )
  enddo
enddo
if (type_mesh == mesh_tri4) then
do i = 1, nx
  do j = 1, ny
    iv  = (i-1)*(ny+1)+j
    ivc = (nx+1)*(ny+1)+(i-1)*ny+j
    umesh%mesh%vertex(ivc, 1,1) = .25_krp*(umesh%mesh%vertex(iv, 1,1)      + umesh%mesh%vertex(iv+1, 1,1)    &
                                         + umesh%mesh%vertex(iv+ny+1, 1,1) + umesh%mesh%vertex(iv+ny+2, 1,1) )
  enddo
enddo
endif

!------------------------------
! creates elements (iso-i lines)
!------------------------------
select case(type_mesh)
case(mesh_quad)
  print*,'. QUAD elements'
  itype = elem_quad4
  nelem = nx*ny
case(mesh_tri)
  print*,'. TRI elements (QUAD split into 2 TRI)'
  itype = elem_tri3
  nelem = 2*nx*ny
case(mesh_tri4)
  print*,'. TRI elements (QUAD split into 4 TRI)'
  itype = elem_tri3
  nelem = 4*nx*ny
case default
  call cfd_error("unknown mesh type")
endselect

ielem = getindex_genelemvtex(umesh%cellvtex, itype)

if (ielem /= 0) call cfd_error("element section already exists")

call addelem_genelemvtex(umesh%cellvtex)
ielem = umesh%cellvtex%nsection
call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, itype)
nvtex = umesh%cellvtex%elem(ielem)%nvtex

do i = 1, nx
  do j = 1, ny
    ic = (i-1)*ny+j         ! QUAD cell index
    iv = (i-1)*(ny+1)+j     ! bottom left corner
    select case(type_mesh)
    case(mesh_quad)
      umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:nvtex) = (/ iv, iv+(ny+1), iv+(ny+1)+1, iv+1 /)
      umesh%cellvtex%elem(ielem)%ielem   (ic)          = ic
    case(mesh_tri)
      umesh%cellvtex%elem(ielem)%elemvtex(2*ic-1, 1:nvtex) = (/ iv, iv+(ny+1), iv+1 /)
      umesh%cellvtex%elem(ielem)%ielem   (2*ic-1)          = 2*ic-1
      umesh%cellvtex%elem(ielem)%elemvtex(2*ic, 1:nvtex)   = (/ iv+1, iv+(ny+1), iv+(ny+1)+1 /)
      umesh%cellvtex%elem(ielem)%ielem   (2*ic)            = 2*ic
    case(mesh_tri4)
      ivc = (nx+1)*(ny+1)+(i-1)*ny+j
      umesh%cellvtex%elem(ielem)%elemvtex(4*ic-3, 1:nvtex) = (/ iv, iv+(ny+1), ivc /)
      umesh%cellvtex%elem(ielem)%ielem   (4*ic-3)          = 4*ic-3
      umesh%cellvtex%elem(ielem)%elemvtex(4*ic-2, 1:nvtex) = (/ iv+(ny+1), iv+(ny+1)+1, ivc /)
      umesh%cellvtex%elem(ielem)%ielem   (4*ic-2)          = 4*ic-2
      umesh%cellvtex%elem(ielem)%elemvtex(4*ic-1, 1:nvtex) = (/ iv+(ny+1)+1, iv+1, ivc /)
      umesh%cellvtex%elem(ielem)%ielem   (4*ic-1)          = 4*ic-1
      umesh%cellvtex%elem(ielem)%elemvtex(4*ic  , 1:nvtex) = (/ iv+1, iv, ivc /)
      umesh%cellvtex%elem(ielem)%ielem   (4*ic  )          = 4*ic
    endselect
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

call typhon_openwrite(iunit, trim(filename), deftyphon, 1)

call typhonwrite_ustmesh(deftyphon, umesh)

!------------------------------
! close file and end program

call close_io_unit(iunit)
print*,'done.'

endprogram
!------------------------------------------------------------------------------!
! Changes
!
! May  2010: created, write cartesian 100x100 cells mesh of 1x1 box
! Apr  2011: command line parameters
!------------------------------------------------------------------------------!

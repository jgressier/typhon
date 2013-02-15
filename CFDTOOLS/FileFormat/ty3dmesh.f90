!------------------------------------------------------------------------------!
! TY3DMESH
! creates an H structured mesh 
!
!------------------------------------------------------------------------------!
program ty3dmesh

use IOCFD
use IO_UNIT
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use FTNARGS

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs, iunit
character(len=256) :: filename, str_opt, str_val
type(st_deftyphon) :: deftyphon
integer(kip)       :: nx, ny, nz
real(krp)          :: lx, ly, lz
type(st_ustmesh)   :: umesh
type(v3d), dimension(:,:,:), pointer :: vertex
type(st_elemvtex), pointer :: elem
integer(kpp)       :: itype, ielem, type_mesh
!---------------------------
logical, parameter :: lincr = .TRUE.
integer(kip)       :: ierr
!---------------------------
integer(kip)       :: i, j, k, iv, ivc, ic, nelem, nvtex, iarg, offset, dim, iloc
!------------------------------------------------------------------------------!
integer(kpp), parameter :: mesh_hexa = 1
logical                 :: fileread

call print_cfdtools_header("TY3DMESH")

!------------------------------
! parse arguments

! default values

lx = 1._krp
ly = 1._krp
lz = 1._krp
nx = 50
ny = 50
nz = 50
filename  = ""
type_mesh = mesh_hexa

nargs    = command_argument_count()
iarg     = 1

fileread = .FALSE.

do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, lincr)
  select case(str_opt)
  ! number of cells in x direction
  case ("-nx")
    ! read integer
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, nx, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("expected integer after '-nx', found '"//trim(str_val)//"'")
  ! number of cells in y direction
  case ("-ny")
    ! read integer
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, ny, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("expected integer after '-ny', found '"//trim(str_val)//"'")
  ! number of cells in z direction
  case ("-nz")
    ! read integer
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, nz, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("expected integer after '-nz', found '"//trim(str_val)//"'")
  ! length in x direction
  case ("-lx")
    ! read real
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, lx, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("real expected after '"//trim(str_opt)//"'"// &
                                ", found '"//trim(str_val)//"'")
  ! length in y direction
  case ("-ly")
    ! read real
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, ly, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("real expected after '"//trim(str_opt)//"'"// &
                                ", found '"//trim(str_val)//"'")
  ! length in z direction
  case ("-lz")
    ! read real
    if (iarg>nargs) call cfd_error("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, lz, lincr, ierr, str_val)
    if (ierr/=0) call cfd_error("real expected after '"//trim(str_opt)//"'"// &
                                ", found '"//trim(str_val)//"'")
  ! mesh types
  case ("-hexa")
    type_mesh = mesh_hexa
  case default
    if (fileread) then
      call cfd_error("too many filenames ("//trim(filename)// &
                                       ", "//trim(str_opt)//")")
    endif
    filename = basename(trim(str_opt), xtyext_mesh)
    fileread = .TRUE.
  endselect
enddo

!------------------------------------------------------------
! default: creates a 50x50x50 uniform mesh in 1x1x1 box
!------------------------------------------------------------

select case(type_mesh)
case(mesh_hexa)
  print*,'creating '//trim(strof(nx))//'x'//trim(strof(ny))//'x'//trim(strof(nz))//' 3D hexa mesh'
case default
  call cfd_error("unknown mesh type")
endselect

if (filename == "") then
  print*,"command line: ty3dmesh [options] filename.tym"
  print*
  print*,"available options:"
  print*
  print*,"  -nx 50    : number of I-cells"
  print*,"  -ny 50    : number of J-cells"
  print*,"  -nz 50    : number of K-cells"
  print*
  print*,"  -lx 1     : domain length"
  print*,"  -ly 1     : domain height"
  print*,"  -lz 1     : domain depth"
  print*
  print*,"  -hexa     : generates hexa (default)"
  print*
  print*,repeat('-',40)
  call cfd_error("missing file name")
else
  filename = trim(filename)//"."//xtyext_mesh
endif

call init_ustmesh(umesh, 1)

!------------------------------
! creates VERTICES
!------------------------------
print*,'. vertices'

select case(type_mesh)
case(mesh_hexa)
  umesh%mesh%nvtex  = (nx+1)*(ny+1)*(nz+1)
case default
  call cfd_error("unknown mesh type")
endselect

umesh%nvtex       = umesh%mesh%nvtex                   ! nb of vertices (redundant)

allocate(umesh%mesh%vertex(1:umesh%mesh%nvtex, 1, 1))
vertex => umesh%mesh%vertex
do i = 1, nx+1
  do j = 1, ny+1
    do k = 1, nz+1
      iv = unsnode(i, j, k)
      vertex(iv, 1, 1) = v3d( (i-1)*(lx/nx), (j-1)*(ly/ny), (k-1)*(lz/nz) )
    enddo
  enddo
enddo

!------------------------------
! creates elements 
!------------------------------
select case(type_mesh)
case(mesh_hexa)
  print*,'. HEXA elements'
  itype = elem_hexa8
  nelem = nx*ny*nz
case default
  call cfd_error("unknown mesh type")
endselect

ielem = getindex_genelemvtex(umesh%cellvtex, itype)

if (ielem /= 0) call cfd_error("element section already exists")

call addelem_genelemvtex(umesh%cellvtex)
ielem = umesh%cellvtex%nsection
call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, itype)
elem => umesh%cellvtex%elem(ielem)
nvtex = elem%nvtex

do i = 1, nx
  do j = 1, ny
    do k = 1, nz
      ic = ((i-1)*ny+j-1)*nz+k             ! HEXA cell index
      select case(type_mesh)
      case(mesh_hexa)
        elem%ielem   (ic)    = ic
        elem%elemvtex(ic, 1) = unsnode(i  , j  , k  ) 
        elem%elemvtex(ic, 2) = unsnode(i+1, j  , k  ) 
        elem%elemvtex(ic, 3) = unsnode(i+1, j+1, k  ) 
        elem%elemvtex(ic, 4) = unsnode(i  , j+1, k  ) 
        elem%elemvtex(ic, 5) = unsnode(i  , j  , k+1) 
        elem%elemvtex(ic, 6) = unsnode(i+1, j  , k+1) 
        elem%elemvtex(ic, 7) = unsnode(i+1, j+1, k+1)
        elem%elemvtex(ic, 8) = unsnode(i  , j+1, k+1)
      endselect
    enddo
  enddo
enddo

!------------------------------
! creates FACES elements (ONLY boundaries) & BC marks
!------------------------------
print*,'. BC faces'

nelem = 2*(nx*ny)+2*(nx*nz)+2*(ny*nz)
nvtex = 4                                  ! faces are quads
call new(umesh%facevtex, nelem, nvtex) 
umesh%nface     = nelem
umesh%nface_lim = nelem

!------------------------------
! BC marks
!------------------------------
print*,'. BC marks (IMIN, IMAX, JMIN, JMAX)'

call createboco(umesh, 6)       ! creates 6 boco (IMIN, IMAX, JMIN, JMAX, KMIN, KMAX)

offset = 0
dim    = ny*nz
call new_ustboco(umesh%boco(1), "IMIN", dim)
call new_ustboco(umesh%boco(2), "IMAX", dim)
do j = 1, ny
  do k = 1, nz
    iloc = (j-1)*nz+k
    ! IMIN faces
    umesh%facevtex%fils(offset+iloc, 1:nvtex) = &
      (/ unsnode(1,j,k), unsnode(1,j,k+1), unsnode(1,j+1,k+1), unsnode(1,j+1,k) /) 
    ! IMAX faces
    umesh%facevtex%fils(offset+dim+iloc, 1:nvtex) = &
      (/ unsnode(nx+1,j,k), unsnode(nx+1,j,k+1), unsnode(nx+1,j+1,k+1), unsnode(nx+1,j+1,k) /) 
    ! MARKS
    umesh%boco(1)%iface(iloc) = offset+iloc
    umesh%boco(2)%iface(iloc) = offset+dim+iloc
  enddo
enddo
offset = offset + 2*dim
dim    = nx*nz
call new_ustboco(umesh%boco(3), "JMIN", dim)
call new_ustboco(umesh%boco(4), "JMAX", dim)
do i = 1, nx
  do k = 1, nz
    iloc = (i-1)*nz+k
    ! JMIN faces
    umesh%facevtex%fils(offset+iloc, 1:nvtex) = &
      (/ unsnode(i,1,k), unsnode(i,1,k+1), unsnode(i+1,1,k+1), unsnode(i+1,1,k) /) 
    ! JMAX faces
    umesh%facevtex%fils(offset+dim+iloc, 1:nvtex) = &
      (/ unsnode(i,ny+1,k), unsnode(i,ny+1,k+1), unsnode(i+1,ny+1,k+1), unsnode(i+1,ny+1,k) /) 
    ! MARKS
    umesh%boco(3)%iface(iloc) = offset+iloc
    umesh%boco(4)%iface(iloc) = offset+dim+iloc
  enddo
enddo
offset = offset + 2*dim
dim    = nx*ny
call new_ustboco(umesh%boco(5), "KMIN", dim)
call new_ustboco(umesh%boco(6), "KMAX", dim)
do i = 1, nx
  do j = 1, ny
    iloc = (i-1)*ny+j
    ! KMIN faces
    umesh%facevtex%fils(offset+iloc, 1:nvtex) = &
      (/ unsnode(i,j,1), unsnode(i+1,j,1), unsnode(i+1,j+1,1), unsnode(i,j+1,1) /) 
    ! KMAX faces
    umesh%facevtex%fils(offset+dim+iloc, 1:nvtex) = &
      (/ unsnode(i,j,nz+1), unsnode(i+1,j,nz+1), unsnode(i+1,j+1,nz+1), unsnode(i,j+1,nz+1) /) 
    ! MARKS
    umesh%boco(5)%iface(iloc) = offset+iloc
    umesh%boco(6)%iface(iloc) = offset+dim+iloc
  enddo
enddo

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

contains

integer function unsnode(i, j, k)
implicit none
integer :: i, j, k

unsnode = ((i-1)*(ny+1)+(j-1))*(nz+1)+k   ! nx, ny, nz are public number of cells in each direction

endfunction unsnode

endprogram
!------------------------------------------------------------------------------!
! Changes
!
! May  2010: created, write cartesian 100x100 cells mesh of 1x1 box
! Apr  2011: command line parameters
!------------------------------------------------------------------------------!

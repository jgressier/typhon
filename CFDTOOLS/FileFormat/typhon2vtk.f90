!------------------------------------------------------------------------------!
! TYPHON2VTK
! convert TYPHON mesh & solution to VTK ascii solution
!
!------------------------------------------------------------------------------!
program typhon2vtk

use IOCFD
use IO_UNIT
use STRING
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use TYFMT_SOL
use VTKFMT
use VTKMESH
use VTKSOL

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=160) :: inputfile, outputfile, str_opt, str_val
integer(kpp)       :: fileformat
!------------------------------------------------------------------------------!
type(st_deftyphon)     :: deftyphon
type(st_defvtk)        :: defvtk
type(st_ustmesh)       :: umesh
type(st_genericfield)  :: gfield
!---------------------------
integer            :: iunit1, iunit2
integer(kip)       :: i, j, iv, ivc, ic, nelem, nvtex, iarg
!------------------------------------------------------------------------------!
integer(kpp), parameter :: mesh_quad = 1
integer(kpp), parameter :: mesh_tri  = 2
integer(kpp), parameter :: mesh_tri4 = 4

call print_cfdtools_header("typhon2vtk")

!------------------------------
! parse arguments

! default values

inputfile  = ""
outputfile = ""
fileformat = vtk_bin

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call get_command_argument(iarg, str_opt)
  iarg = iarg + 1
  if ((str_opt == "-ascii").or.(str_opt == "-asc"))  then
    fileformat = vtk_asc
    !call get_command_argument(iarg, str_val)
    !iarg = iarg + 1
    !read(str_val, *) nx
  elseif ((str_opt == "-binary").or.(str_opt == "-bin"))  then
    fileformat = vtk_bin
    !call get_command_argument(iarg, str_val)
    !iarg = iarg + 1
    !read(str_val, *) nx
  elseif (str_opt == "-o")  then
    call get_command_argument(iarg, outputfile)
    iarg = iarg + 1
  else
    inputfile = basename(trim(str_opt), xtyext_sol)
  endif
enddo

if (inputfile == "") then
  print*,"command line: typhon2vtk [options] inputfile.tys"
  print*,"available options:"
  print*,"  -bin / -binary : binary    output (default)"
  print*,"  -asc / -ascii  : formatted output"
  print*,"  -o basename    : define basename of output file"
  call cfd_error("missing file name")
endif

if (outputfile == "") then
  outputfile = inputfile
else 
  outputfile = basename(trim(outputfile), "vtk")
endif

!------------------------------------------------------------
! define mesh parameters


!------------------------------------------------------------
! read mesh and solution
!------------------------------------------------------------

print*,'* Opening TYPHON file: '//trim(inputfile)//"."//xtyext_sol

iunit1 = getnew_io_unit()
call typhon_openread(iunit1, trim(inputfile)//"."//xtyext_sol, deftyphon)

call typhonread_ustmesh(deftyphon%defxbin, umesh)

call delete_ustmesh_subelements(umesh)

call typhonread_sol(deftyphon%defxbin, umesh, gfield)

call close_io_unit(iunit1)

!------------------------------------------------------------
! write mesh and solution
!------------------------------------------------------------

print*,'* Opening VTK file: '//trim(outputfile)//".vtk"

iunit2 = getnew_io_unit()
select case(fileformat)
case(vtk_asc)
  call vtkasc_openwrite(iunit2, trim(outputfile)//".vtk", defvtk)
case(vtk_bin)
  call vtkbin_openwrite(iunit2, trim(outputfile)//".vtk", defvtk)
case default
  call cfd_error("unexpected file format")
endselect

call writevtk_ustmesh(defvtk, umesh)

call writevtk_sol(defvtk, umesh, gfield)

!------------------------------
! close file and end program

call close_io_unit(iunit2)
print*,'done.'

endprogram
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created
!------------------------------------------------------------------------------!

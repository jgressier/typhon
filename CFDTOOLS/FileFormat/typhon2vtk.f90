!> @addtogroup Program
!------------------------------------------------------------------------------!
!> @ingroup Program
!> @brief converts typhon internal solution into VTK format
!> features
!> - ascii or binary VTK legacy format
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
character(len=160) :: basefile, inputfile, outputfile, str_opt, str_val
integer(kpp)       :: fileformat
!------------------------------------------------------------------------------!
type(st_deftyphon)     :: deftyphon
type(st_defvtk)        :: defvtk
type(st_ustmesh)       :: umesh
type(st_genericfield)  :: gfield
logical                :: onlymesh
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
onlymesh   = .false.
fileformat = vtk_bin

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call get_command_argument(iarg, str_opt)
  iarg = iarg + 1
  if ((str_opt == "-ascii").or.(str_opt == "-asc"))  then
    fileformat = vtk_asc
  elseif ((str_opt == "-binary").or.(str_opt == "-bin"))  then
    fileformat = vtk_bin
  elseif (str_opt == "-onlymesh") then
    onlymesh = .true.
  elseif (str_opt == "-o")  then
    call get_command_argument(iarg, outputfile)
    iarg = iarg + 1
  else
    inputfile = trim(str_opt) !basename(trim(str_opt), xtyext_sol)
  endif
enddo

if (inputfile == "") then
  print*,"command line: typhon2vtk [options] inputfile.[tym|tys]"
  print*,"available options:"
  print*,"  -bin / -binary : binary    output (default)"
  print*,"  -asc / -ascii  : formatted output"
  print*,"  -onlymesh      : only mesh (default if input file is .tym)"
  print*,"  -o basename    : define basename of output file"
  call cfd_error("missing file name")
endif

if (index(inputfile, '.'//xtyext_sol) ==  len_trim(inputfile)-len_trim(xtyext_sol)) then
  basefile = basename(trim(inputfile), xtyext_sol)
elseif (index(inputfile, '.'//xtyext_mesh) ==  len_trim(inputfile)-len_trim(xtyext_mesh)) then
  basefile = basename(trim(inputfile), xtyext_mesh)
  onlymesh = .true.
else
  call cfd_error("missing or unrecognized file name: "//trim(inputfile))
endif

if (trim(outputfile) == "") then
  outputfile = basefile
else 
  outputfile = basename(trim(outputfile), "vtk")
endif

!------------------------------------------------------------
! define mesh parameters


!------------------------------------------------------------
! read mesh and solution
!------------------------------------------------------------

print*,'* Opening TYPHON file: '//trim(inputfile)

call typhon_openread(trim(inputfile), deftyphon)

call typhonread_ustmesh(deftyphon, umesh)

call delete_ustmesh_subelements(umesh)

if (.not.onlymesh) call typhonread_sol(deftyphon, umesh, gfield)

call typhon_close(deftyphon)

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

if (.not.onlymesh) call writevtk_sol(defvtk, umesh, gfield)

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

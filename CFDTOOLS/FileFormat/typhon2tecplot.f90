!------------------------------------------------------------------------------!
! TYPHON2tecplot
! convert TYPHON mesh & solution to tecplot ascii solution
!
!------------------------------------------------------------------------------!
program typhon2tecplot

use IOCFD
use IO_UNIT
use STRING
use VEC3D
use USTMESH
use XBIN_IO
use TYPHON_FMT
use TYFMT_MESH
use TYFMT_SOL
use TECFMT
use TECMESHSOL

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=160) :: inputfile, outputfile, str_opt, str_val
!------------------------------------------------------------------------------!
type(st_deftyphon)     :: deftyphon
type(st_deftec)        :: deftec
type(st_ustmesh)       :: umesh
type(st_genericfield)  :: gfield
!---------------------------
integer            :: iunit1, iunit2
integer(kip)       :: i, j, iv, ivc, ic, nelem, nvtex, iarg
!------------------------------------------------------------------------------!

call print_cfdtools_header("typhon2tecplot")

!------------------------------
! parse arguments

! default values

inputfile  = ""
outputfile = ""

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call get_command_argument(iarg, str_opt)
  iarg = iarg + 1
  if (str_opt == "-o")  then
    call get_command_argument(iarg, outputfile)
    iarg = iarg + 1
  else
    inputfile = basename(trim(str_opt), xtyext_sol)
  endif
enddo

if (inputfile == "") then
  print*,"command line: typhon2tec [options] inputfile.tys"
  print*,"available options:"
  print*,"  -o basename    : define basename of output file"
  call cfd_error("missing file name")
endif

if (outputfile == "") then
  outputfile = inputfile
else 
  outputfile = basename(trim(outputfile), "tec")
endif

!------------------------------------------------------------
! define mesh parameters


!------------------------------------------------------------
! read mesh and solution
!------------------------------------------------------------

print*,'* Opening TYPHON file: '//trim(inputfile)//"."//xtyext_sol

call typhon_openread(trim(inputfile)//"."//xtyext_sol, deftyphon)

call typhonread_ustmesh(deftyphon, umesh)

call delete_ustmesh_subelements(umesh)

call typhonread_sol(deftyphon, umesh, gfield)

call typhon_close(deftyphon)

!------------------------------------------------------------
! write mesh and solution
!------------------------------------------------------------

print*,'* Opening TEC file: '//trim(outputfile)//".plt"

iunit2 = getnew_io_unit()

call tec_openwrite(iunit2, trim(outputfile)//".plt", deftec)

call tecwrite_allzoneheader(deftec, umesh, gfield, 1)

print*,'> write mesh'
call tecwrite_ustmesh(deftec, umesh)

print*,'> write solution'
call tecwrite_sol(deftec, umesh, gfield)

!------------------------------
! close file and end program

call close_io_unit(iunit2)
print*,'done.'

endprogram typhon2tecplot
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created
!------------------------------------------------------------------------------!

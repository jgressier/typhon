!> @addtogroup Program
!------------------------------------------------------------------------------!
!> @ingroup Program
!> @brief convert a cgns mesh to typhon mesh
!> features
!> - moves nodes according to FCT functions
!------------------------------------------------------------------------------!
program cgns2typhon

use IOCFD
use FTNARGS
use STRING
use VEC3D
use USTMESH
use XBIN_IO
use CGNS_STRUCT
use TYPHON_FMT
use TYFMT_MESH
use FCT_PARSER
use FCT_EVAL
use MESHPARAMS
use MESHCONNECT
use MESHGEOM

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=256) :: inputfile, outputfile, filebase, str_opt, str_val
integer            :: ltf
character(len=*), parameter :: inputext_mesh = "cgns"
!------------------------------------------------------------------------------!
type(st_deftyphon) :: deftyphon
type(st_ustmesh)   :: umesh
type(mnu_mesh)     :: defmesh
!---------------------------
integer(kip)       :: iarg, nfread
logical            :: needgeom
!------------------------------------------------------------------------------!

call print_cfdtools_header("cgns2typhon")

!------------------------------
! parse arguments

! default values

inputfile   = ""
outputfile  = ""
nfread      = 0
needgeom    = .true.

defmesh%icgnsbase = 1
defmesh%icgnszone = 1

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, .true.)
  select case(str_opt)
  case("-o")
    call read_command_argument(iarg, str_val, .true.)
    outputfile = trim(basename(trim(str_val), xtyext_mesh))
  case("-b","--base")
    call read_command_argument(iarg, str_val, .true.)
    read(str_val,*) defmesh%icgnsbase
  case("-z","--zone")
    call read_command_argument(iarg, str_val, .true.)
    read(str_val,*) defmesh%icgnszone
  case default
    inputfile = trim(str_opt)
    write(6,'(3a)') "* File: `",trim(inputfile),"'"
    nfread  = nfread + 1
  endselect
enddo

if (inputfile == "") then
  call cfdtool_error("Missing filename")
endif

if (nfread .gt. 1) then
  call cfdtool_error("Too many filenames")
endif

ltf = len_trim(inputfile)
! If inputfile ends with suffix "."//inputext_mesh
if ( inputfile(max(1,ltf-len(inputext_mesh)):ltf) == "."//inputext_mesh ) then
  ! Remove suffix for filebase
  filebase = inputfile(1:ltf-len(inputext_mesh)-1)
else
  filebase = inputfile
  ! Add suffix to filebase for inputfile
  inputfile = trim(inputfile)//"."//inputext_mesh
endif

ltf = len_trim(outputfile)
if (outputfile == "") then
  outputfile = trim(filebase)//"."//xtyext_mesh
elseif ( outputfile(ltf-len(xtyext_mesh):ltf) /= "."//xtyext_mesh ) then
  ! Add suffix to outputfile
  outputfile = trim(outputfile)//"."//xtyext_mesh
endif  

!------------------------------------------------------------
! read mesh
!------------------------------------------------------------

defmesh%filename = trim(inputfile)

call importcgns_mesh(defmesh, umesh)

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------

write(6,'()')
write(6,'(a)') "* Writing TYPHON file: "//trim(outputfile)
write(6,'(a)') "* (IS FILE COMPLETE ? (BCs ?)) ???"
!------------------------------
! open xbin file

call typhon_openwrite(trim(outputfile), deftyphon, 1)

call typhonwrite_ustmesh(deftyphon, umesh)

!------------------------------
! close files and end program

call typhon_close(deftyphon)

write(6,'(a)') "* Done"

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  write(6,'(a)') 'command line: cgns2typhon [options] <inputfile>[.'//trim(inputext_mesh)//']'
  write(6,'()')
  write(6,'(a)') 'where options are:'
  write(6,'(a)') '  -o <filename>   output typhon mesh (default: <inputfile>.'//trim(xtyext_mesh)//')'
  write(6,'(a)') '  -b|--base nb  base number (default 1)'
  write(6,'(a)') '  -z|--zone nz  zone number (default 1)'
  write(6,'()')
  call cfd_error(str)
endsubroutine cfdtool_error

endprogram cgns2typhon
!------------------------------------------------------------------------------!
! Changes
!
! Jun 2014 : created
!------------------------------------------------------------------------------!

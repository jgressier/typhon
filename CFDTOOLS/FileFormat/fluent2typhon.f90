!> @addtogroup Program
!------------------------------------------------------------------------------!
!> @ingroup Program
!> @brief convert a fluent mesh to typhon mesh
!> features
!> - moves nodes according to FCT functions
!------------------------------------------------------------------------------!
program fluent2typhon

use IOCFD
use FTNARGS
use STRING
use VEC3D
use USTMESH
use XBIN_IO
use FLUENT
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
character(len=256) :: inputfile, outputfile, filename, str_opt, str_val
!------------------------------------------------------------------------------!
type(st_deffluent) :: deffluent
type(st_deftyphon) :: deftyphon
type(st_ustmesh)   :: umesh
type(mnu_mesh)     :: defmesh
!---------------------------
integer(kip)          :: ip, iarg
logical               :: needgeom, fileread
type(st_fctfuncset)   :: fctenv
!------------------------------------------------------------------------------!
! Gilles fluent/readmsh variables
!integer :: i
real*8       :: x(262144),y(262144),z(262144)
integer(kfp) :: elemlist(262144)
character(len=64) :: str
!integer :: sctid
!integer :: spacedim
!integer :: f_zoneid, frstindx, lastindx, ntype, ctype, bctype, ndim
!integer :: iunit1, iunit2
logical :: iend
!------------------------------------------------------------------------------!

call print_cfdtools_header("fluent2typhon")

!------------------------------
! parse arguments

! default values

inputfile   = ""
outputfile  = ""
needgeom    = .true.
fileread    = .false.

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, .true.)
  select case(str_opt)
  case("-o")
    call read_command_argument(iarg, str_val, .true.)
    outputfile = trim(basename(trim(str_val), xtyext_mesh))
  case default
    if (fileread) then
      call cfd_error("too many filenames ("//trim(inputfile)//", "//trim(str_opt)//")")
    endif
    inputfile = trim(str_opt)
    fileread  = .true.
  endselect
enddo

if (inputfile == "") then
  call cfdtool_error("missing filename")
endif

if (outputfile == "") then
  outputfile = trim(inputfile)//xtyext_mesh
endif  

!------------------------------------------------------------
! read mesh
!------------------------------------------------------------

filename = trim(inputfile)

write(6,'(2a)') "* Opening Fluent file: ",trim(filename)

!call typhon_openread(trim(filename), deftyphon)

!call typhonread_ustmesh(deftyphon, umesh)
!!call delete_ustmesh_subelements(umesh)
!call typhon_close(deftyphon)

!------------------------------------------------------------

!write(6,'(a)') "files :"
!write(6,'(2x,a)') "f.msh"
!write(6,'(2x,a)') "cube.msh"
!write(6,'(2x,a)') "cone.msh"
!write(6,'(2x,a)') "tube.msh"
!write(6,'(2x,a)') "chbr.msh X not working"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

defmesh%filename = trim(filename)

call importfluent_mesh(defmesh, umesh)

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------

!filename = trim(outputfile)//"."//xtyext_mesh
write(6,'()')
write(6,'(2a)') "* Writing TYPHON file: ",trim(filename)
write(6,'(a)') "* FILE IS NOT WRITTEN !!!"
!------------------------------
! open xbin file

!call typhon_openwrite(trim(filename), deftyphon, 1)

!call typhonwrite_ustmesh(deftyphon, umesh)

!------------------------------
! close files and end program

!call typhon_close(deftyphon)
write(6,'(a)') "* Done"

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  write(6,'(a)') 'command line: fluent2typhon [options] inputfile[.msh]'
  write(6,'()')
  write(6,'(a)') 'where options are:'
  write(6,'(a)') '  -o filename : output typhon mesh (default: inputfile.tym)'
  call cfd_error(str)
endsubroutine cfdtool_error

endprogram fluent2typhon
!------------------------------------------------------------------------------!
! Changes
!
! June 2014: created
!------------------------------------------------------------------------------!

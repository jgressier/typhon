!> @addtogroup Program
!------------------------------------------------------------------------------!
!> @ingroup Program
!> @brief convert a fluent mesh to typhon mesh
!> features
!> - moves nodes according to FCT functions
!------------------------------------------------------------------------------!
program tymorph

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
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
type(mnu_mesh)             :: defmesh
!---------------------------
integer(kip)          :: ip, iarg
logical               :: needgeom, fileread
type(st_fctfuncset)   :: fctenv
!------------------------------------------------------------------------------!
! Gilles fluent/readmsh variables
integer :: i,k
integer :: mypos
real*8       :: x(262144),y(262144),z(262144)
integer(kpf) :: elemlist(262144)
character(len=64) :: filename
character(len=64) :: str
character(len=1)  :: c
integer :: sctid
integer :: spacedim
integer :: f_zoneid, frstindx, lastindx, ntype, ctype, bctype, nd
integer , parameter :: iunit1 = 11
integer , parameter :: iunit2 = 12
integer :: iend
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
    fileread  = .TRUE.
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

filename = trim(inputfile)//"."//xtyext_mesh

print*,'* Opening Fluent file: '//trim(filename)

call typhon_openread(trim(filename), deftyphon)

call typhonread_ustmesh(deftyphon, umesh)
!call delete_ustmesh_subelements(umesh)
call typhon_close(deftyphon)

!------------------------------------------------------------
! morph mesh

write(6,'(a)') "files :"
write(6,'(2x,a)') "f.msh"
write(6,'(2x,a)') "cube.msh"
write(6,'(2x,a)') "cone.msh"
write(6,'(2x,a)') "tube.msh"
write(6,'(2x,a)') "chbr.msh"

write(6,'(a,$)') "Filename ? "
read(5,*) filename

! x = 1.0
! 
! OPEN(iunit1, FILE="mystream", STATUS="REPLACE", ACCESS="STREAM", FORM="FORMATTED")
! WRITE(iunit1, "(4A)") "frst line", NEW_LINE("x"), "second line"
! CLOSE(iunit1)
! OPEN(iunit1, FILE="mystream", POSITION="APPEND", ACCESS="STREAM", FORM="UNFORMATTED")
! do i = 1, 4
!   y = sin(x*i/16.0d0)
!   write(iunit1) y ; write(6,*) y
! enddo
! INQUIRE(iunit1, POS=mypos)
! CLOSE(iunit1)
! OPEN(iunit1, FILE="mystream", POSITION="APPEND", ACCESS="STREAM", FORM="FORMATTED")
! WRITE(iunit1, "(A)") "last line"
! WRITE(iunit1, "(A)") "very last line"
! CLOSE(iunit1)
! 
! OPEN(iunit1, FILE="mystream", ACCESS="STREAM", FORM="FORMATTED")
! OPEN(iunit2, FILE="mystream", ACCESS="STREAM", FORM="UNFORMATTED")
! READ (iunit1, '(A)') str ; write(6,*) str
! READ (iunit1, '(A)') str ; write(6,*) str
! INQUIRE(iunit1, POS=mypos)
! write(6,*) mypos
! READ (iunit2, POS=mypos)
! INQUIRE(iunit2, POS=mypos)
! write(6,*) mypos
! do i = 1, 4
!   read(iunit2) x ; write(6,*) x
! enddo
! INQUIRE(iunit1, POS=mypos)
! write(6,*) mypos
! INQUIRE(iunit2, POS=mypos)
! write(6,*) mypos
! read(iunit1, '(a32,$)') str ; write(6,*) str
! !READ (iunit1, '()', ADVANCE='NO', POS=mypos)
! READ (iunit1, '(A)') str ; write(6,*) str
! CLOSE(iunit1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(iunit1, FILE=filename, ACCESS="STREAM", FORM="FORMATTED")
OPEN(iunit2, FILE=filename, ACCESS="STREAM", FORM="UNFORMATTED", &
     CONVERT='LITTLE_ENDIAN')

indopt = 1
namopt = "main_____"
hedopt = 0

espion = 0

read(iunit2) c
!
do while (.TRUE.)
  stropt = " "
  indopt = 1
  call fluent_get_sct_id(iunit2, sctid, c)
  select case(sctid)
  case(fluent_sct_comment)      ! Section : Comment
    call fluent_get_sct_comment(iunit2, c)
  case(fluent_sct_header)       ! Section : Header
    call fluent_get_sct_header(iunit2, c)
  case(fluent_sct_dim)          ! Section : Dimension
    call fluent_get_sct_dim(iunit2, spacedim, c)
  case (fluent_sct_ignore1)     ! Section : ignore1 (ignored)
    call fll_getlist(iunit2, str, c)
    write(6,'(3a)') "ignore1   : '",trim(str),"'"
  case(fluent_sct_nodes, &
       fluent_sct_spbinnodes, &
       fluent_sct_dpbinnodes)   ! Section : Nodes
    call fluent_get_sct_nodes(iunit2, sctid, f_zoneid, &
                              x, y, z, &
                              frstindx, lastindx, ntype, nd, spacedim, c)
  case(fluent_sct_cells, &
       fluent_sct_bincells)     ! Section : Cells
    call fluent_get_sct_cells(iunit2, sctid, f_zoneid, &
                              elemlist, &
                              frstindx, lastindx, ctype, c)
  case(fluent_sct_faces, &
       fluent_sct_binfaces)     ! Section : Cells
    call fluent_get_sct_faces(iunit2, sctid, f_zoneid, &
                              elemlist, &
                              frstindx, lastindx, bctype, c)
  case(fluent_sct_zone, &
       fluent_sct_zoneb)        ! Section : Zone
    call fll_getlist(iunit2, str, c)
    call fll_getlist(iunit2, str, c)
  case default                  ! Section : UNKNOWN
    write(6,'(a)') "Error !"
    write(str,'(i)') sctid
    call affline("sctid="//trim(str))
    call affline("c='"//c//"'")
    call stopp()
  endselect
  call fluent_get_sct_end(iunit2, iend, c)
  if ( iend .ne. 0 ) then
    exit
  endif
enddo
!
CLOSE(iunit1)
CLOSE(iunit2)

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------
!
!filename = trim(outputfile)//"."//xtyext_mesh
!print*
!print*,'* Writing TYPHON file: '//trim(filename)
!!------------------------------
!! open xbin file
!
!call typhon_openwrite(trim(filename), deftyphon, 1)
!
!call typhonwrite_ustmesh(deftyphon, umesh)
!
!!------------------------------
!! close files and end program
!
!call typhon_close(deftyphon)
!print*,'done.'

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  print*,'command line: tymorph [options] inputfile[.tym]'
  print*
  print*,'where options are:'
  print*,'  -o filename      : output typhon mesh (default: morphed_inputfile)'
  call cfd_error(str)
endsubroutine cfdtool_error

endprogram fluent2typhon
!------------------------------------------------------------------------------!
! Changes
!
! June 2014: created
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! TYPART
!
!------------------------------------------------------------------------------!
program typart

use IOCFD
use FTNARGS
use STRING
use USTMESH
use TYPHON_FMT
use TYFMT_MESH
use TYFMT_PART
use MESHCONNECT
use MESHPART

implicit none

!------------------------------------------------------------------------------!
integer            :: nargs
character(len=256) :: inputfile, outputfile, filename, str_opt, str_val
!------------------------------------------------------------------------------!
type(st_deftyphon)         :: deftyphon
type(st_deftypart)         :: deftypart
type(st_ustmesh)           :: umesh
type(mnu_mesh)             :: defmesh
integer(kpp)               :: part_method
!---------------------------
integer(kip)              :: ip, iarg, npart, nci
logical                   :: needgeom, fileread
integer(kip), allocatable :: partition(:)  ! result of partition
!------------------------------------------------------------------------------!

call print_cfdtools_header("typart")

!------------------------------
! parse arguments

! default values

inputfile   = ""
outputfile  = ""
npart       = 0
needgeom    = .true.
fileread    = .false.
part_method = part_auto

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, .true.)
  select case(str_opt)
  case("-o")
    call read_command_argument(iarg, str_val, .true.)
    outputfile = trim(basename(trim(str_val), xtyext_mesh))
  case("-n")
    call read_command_argument(iarg, npart, .true.)
  case("-recursive")
    part_method = part_metisrecursive
  case("-kway")
    part_method = part_metiskway
  case("-auto")
    part_method = part_auto
  case default
    if (fileread) then
      call cfd_error("too many filenames ("//trim(inputfile)//", "//trim(str_opt)//")")
    endif
    inputfile = basename(trim(str_opt), xtyext_mesh)
    fileread  = .TRUE.
  endselect
enddo

if (inputfile == "") then
  call cfdtool_error("missing filename")
endif

if (outputfile == "") then
  outputfile = trim(inputfile)//"."//trim(strof(npart))//"."//xtyext_part
endif  

if (npart < 2) then
  call cfdtool_error("needs at least 2 parts")
endif

!------------------------------------------------------------
! read mesh
!------------------------------------------------------------

filename = trim(inputfile)//"."//xtyext_mesh

print*,'* Opening TYPHON file: '//trim(filename)

call typhon_openread(trim(filename), deftyphon)

call typhonread_ustmesh(deftyphon, umesh)
!call delete_ustmesh_subelements(umesh)
call typhon_close(deftyphon)

!------------------------------------------------------------
! compute partition

print*,'* computing mesh graph...'
call create_face_connect(.false., umesh)

print*,'* mesh partionning...'

nci = umesh%ncell_int
allocate(partition(nci))

call cfd_print("> compute partition: "//trim(strof(npart))//" parts")

call ustmesh_partition(part_method, umesh, npart, nci, partition)

!------------------------------------------------------------
! Create partition file
!------------------------------------------------------------

print*
print*,'* Writing partition file: '//trim(outputfile)
!------------------------------
! open xbin file

call typart_openwrite(trim(outputfile), deftypart, nci, npart)

call typart_writepartition(deftypart, partition)

!------------------------------
! close files and end program

call typart_close(deftypart)
print*,'done.'

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  print*,'command line: typart [options] inputfile[.tym]'
  print*
  print*,'where options are:'
  print*,'  -n npart         : number of partition'
  print*,'  -o base          : output basename (default:basename of inputfile)'
  print*,'  -kway            : force Metis k-way method'
  print*,'  -recursive       : force Metis recursive method'
  print*,'  -auto            : automatic partition method (default)'
  print*,'output partition file is base.npart.typ'
  call cfd_error(str)
endsubroutine cfdtool_error

endprogram typart
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2014: created
!------------------------------------------------------------------------------!

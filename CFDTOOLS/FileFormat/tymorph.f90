!------------------------------------------------------------------------------!
! TYMORPH
!
!------------------------------------------------------------------------------!
program tymorph

use IOCFD
use IO_UNIT
use FTNARGS
use STRING
use VEC3D
use USTMESH
use XBIN_IO
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
character(len=256) :: inputfile, outputfile, filename, str_opt, str_val, strmx, strmy, strmz
!------------------------------------------------------------------------------!
type(st_deftyphon)         :: deftyphon
type(st_ustmesh)           :: umesh
type(mnu_mesh)             :: defmesh
!---------------------------
integer               :: iunit
type(st_fct_node)     :: morphx, morphy, morphz
integer(kip)          :: ip, iarg
logical               :: needgeom, fileread
type(st_fctfuncset)   :: fctenv
!------------------------------------------------------------------------------!

call print_cfdtools_header("tymorph")

!------------------------------
! parse arguments

! default values

inputfile   = ""
outputfile  = ""
strmx       = "x"
strmy       = "y"
strmz       = "z"
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
  case("-fx")
    call read_command_argument(iarg, strmx, .true.)
  case("-fy")
    call read_command_argument(iarg, strmy, .true.)
  case("-fz")
    call read_command_argument(iarg, strmz, .true.)
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
  outputfile = "morphed_"//trim(inputfile)
endif  

print*,'. new X is '//trim(strmx)
call string_to_funct(strmx, morphx, iarg)
print*,'. new Y is '//trim(strmy)
call string_to_funct(strmy, morphy, iarg)
print*,'. new Z is '//trim(strmz)
call string_to_funct(strmz, morphz, iarg)

!------------------------------------------------------------
! read mesh
!------------------------------------------------------------

filename = trim(inputfile)//"."//xtyext_mesh

print*,'* Opening TYPHON file: '//trim(filename)

iunit = getnew_io_unit()
call typhon_openread(iunit, trim(filename), deftyphon)

call typhonread_ustmesh(deftyphon, umesh)
!call delete_ustmesh_subelements(umesh)
call close_io_unit(iunit)

!------------------------------------------------------------
! morph mesh

  print*,'  mesh morphing computation...'
  call new_fctfuncset(fctenv)
  call morph_vertex(fctenv, umesh%mesh, morphx, morphy, morphz)
  print*,'  done'
  call delete_fctfuncset(fctenv)
  call delete_fct_node(morphx)
  call delete_fct_node(morphy)
  call delete_fct_node(morphz)

!------------------------------------------------------------
! Create mesh file
!------------------------------------------------------------

filename = trim(outputfile)//"."//xtyext_mesh
print*
print*,'* Writing TYPHON file: '//trim(filename)
!------------------------------
! open xbin file

iunit = getnew_io_unit()

call typhon_openwrite(iunit, trim(filename), deftyphon, 1)

call typhonwrite_ustmesh(deftyphon, umesh)

!------------------------------
! close files and end program

call close_io_unit(iunit)
print*,'done.'

contains

!------------------------------------------------------------------------------!
subroutine cfdtool_error(str)
  implicit none
  character(len=*) :: str
  print*,'command line: tymorph [options] inputfile[.tym]'
  print*
  print*,'where options are:'
  print*,'  -o filename      : output typhon mesh (default: morphed_inputfile)'
  print*,'  -fx "expression" : new X coordinate (default is "x")'
  print*,'  -fy "expression" : new Y coordinate (default is "y")'
  print*,'  -fz "expression" : new Z coordinate (default is "z")'
  print*,'"expression"  is a symbolic expression of x, y, z'
  call cfd_error(str)
endsubroutine cfdtool_error

endprogram tymorph
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2013: created
!------------------------------------------------------------------------------!

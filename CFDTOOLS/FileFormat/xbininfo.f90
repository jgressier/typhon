!------------------------------------------------------------------------------!
! TY2DMESH
! creates an H structured mesh
!
!------------------------------------------------------------------------------!
program xbininfo

use IOCFD
use ENDIAN
use XBIN_DATA
use STRING

implicit none

integer, parameter :: tab1 = 17
!------------------------------------------------------------------------------!
integer                     :: nargs       ! number of command line arguments
integer                     :: iunit       ! IO unit
character(len=120)          :: filename    ! xbin file name
type(st_defxbin)            :: defxbin     ! xbin file properties
type(st_xbindatasection)    :: xbindata    ! xbin data section
!------------------------------------------------------------------------------!
integer                     :: idatasec

!------------------------------------------------------------------------------!
call print_cfdtools_header("XBININFO")

select case(native_endianness())
case(endian_little)
  print '(a)','native byte order: little endian'
case(endian_big)
  print '(a)','native byte order: big endian'
case(endian_unknown)
  print '(a)','native byte order: unknown representation'
endselect

!------------------------------
! parse arguments

nargs = command_argument_count()

if (nargs /= 1) then
  call cfd_error("bad number of command line arguments")
else
  call get_command_argument(1, filename)
endif

!------------------------------
! open xbin file

call xbin_openread(trim(filename), defxbin)

print '(3a)',fill('file',        tab1),': ',trim(filename)
print '(6a)',fill('XBIN version',tab1),': ',trim(strof(defxbin%xbin_version)),&
                                   ' (max:',trim(strof(xbin_maxver)),')'
print '(3a)',fill('endianness',  tab1),': ',trim(byteorder_str(defxbin%byteorder))

idatasec = 0
do while (.not.xbin_eof(defxbin))

  idatasec = idatasec + 1
  print '(a,i3,a,$)','DATA SECTION [ ',idatasec,' ]'

  call xbin_readdatahead(defxbin, xbindata)

  print '(2a,$)',' - ',fill('"'//trim(xbindata%name)//'"', 8)
  print '(2a)'  ,' - type : ',trim(strof(xbindata%usertype))
  print '(3a,$)',fill('    parameters', tab1),': ',strofr(xbindata%nparam,4)
  if (xbindata%nparam >= 1) then
    print '(a,$)','  [ '
    print '(i6,$)',xbindata%param(:)
    print '(a,$)',' ]'
  endif
  print '()'
  print '(2a)'  ,fill('    string', tab1),': "'//trim(xbindata%string)//'"'
  print '(3a,$)',fill('    data type', tab1),': ',fill(trim(xbindataname(xbindata)),22)
  if (xbindata%dim == 0) then
    print '(3a)',fill('  - data size', tab1),': ',trim(strof(xbindata%nelem))
  else
    print '(5a)',fill('  - data size', tab1),': ',trim(strof(xbindata%nelem)),'x',trim(strof(xbindata%dim))
  endif

  call xbin_skipdata(defxbin, xbindata)
  call delete_xbindata(xbindata)
enddo

print '(3a)',fill('nb data sections', tab1),': ',trim(strof(idatasec))

!------------------------------
! close file and end program

call xbin_close(defxbin)

endprogram xbininfo
!------------------------------------------------------------------------------!
! Change history
!
! May 2010 :
!------------------------------------------------------------------------------!

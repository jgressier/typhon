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
case(fmt_littleendian)
  print*,'native byte order: little endian'
case(fmt_bigendian)
  print*,'native byte order: big endian'
case(fmt_unknown)
  print*,'native byte order: unknown representation'
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

print*,fill('file',        tab1),': ',trim(filename)
print*,fill('XBIN version',tab1),': ',trim(strof(defxbin%xbin_version)),&
                                 ' (max:',trim(strof(xbin_maxver)),')'
print*,fill('endianness',  tab1),': ',trim(byteorder_str(defxbin%byteorder))

idatasec = 0
do while (.not.xbin_eof(defxbin))

  idatasec = idatasec + 1
  print*,fill('DATA SECTION', tab1),'#',strof(idatasec)
  
  call xbin_readdatahead(defxbin, xbindata)

  print*,fill('  name',       tab1),': ',trim(xbindata%name)
  print*,fill('  user type',  tab1),': ',trim(strof(xbindata%usertype))
  print*,fill('  parameters', tab1),': ',trim(strof(xbindata%nparam))
  if (xbindata%nparam >= 1) then
    print*,fill('', tab1),': ',xbindata%param(:)
  endif
  print*,fill('  string', tab1),': ',trim(xbindata%string)
  print*,fill('  data type', tab1),': ',trim(xbindataname(xbindata))
  if (xbindata%dim == 0) then
    print*,fill('  data size', tab1),': ',trim(strof(xbindata%nelem))
  else
    print*,fill('  data size', tab1),': ',trim(strof(xbindata%nelem)),'x',trim(strof(xbindata%dim))
  endif

  call xbin_skipdata(defxbin, xbindata)
  call delete_xbindata(xbindata)
enddo

print*,fill('nb of data section', tab1),': ',trim(strof(idatasec))

!------------------------------
! close file and end program

call xbin_close(defxbin)

endprogram
!------------------------------------------------------------------------------!
! Changes
!
! May  2010: 
!------------------------------------------------------------------------------!

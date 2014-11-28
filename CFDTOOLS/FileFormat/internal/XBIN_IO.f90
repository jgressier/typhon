!------------------------------------------------------------------------------!
! MODULE : XBIN_IO
!
!------------------------------------------------------------------------------!
module XBIN_IO

use IOCFD
use IO_UNIT
use ENDIAN
use STRING

implicit none

! -- Global Variables -------------------------------------------

integer,           parameter :: xbinver = 2
integer,           parameter :: xbinkpp = 2
integer,           parameter :: xbinkrp = 8
integer,           parameter :: xbinkip = 4
integer,           parameter :: kendian = 4
integer(kendian),  parameter :: endian_test     = 1*256**3 + 2*256**2 + 3*256 + 4  
integer(xbinver),  parameter :: xbin_defaultver = 2
integer(xbinver),  parameter :: xbin_maxver     = 2
character(len=4),  parameter :: xbin_strheader  = "XBIN"
character(len=11), parameter :: xbin_prefix     = "[XBIN lib] "
character(len=3),  parameter :: xbin_endsection = "END"

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Structures
!------------------------------------------------------------------------------!
type st_defxbin
  integer          :: iunit
  integer          :: byteorder
  integer(xbinver) :: xbin_version
endtype st_defxbin

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! XBIN error output
!------------------------------------------------------------------------------!
subroutine xbin_error(str)
implicit none
! -- INPUTS --
character(len=*) :: str
! -- private data --

call cfd_error(xbin_prefix//str) 

endsubroutine xbin_error

!------------------------------------------------------------------------------!
! test end of XBIN file
!------------------------------------------------------------------------------!
logical function xbin_eof(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) , intent(IN) :: defxbin
! -- private data --
  xbin_eof = io_eof(defxbin%iunit)  
endfunction xbin_eof


!------------------------------------------------------------------------------!
! read and write END of section
!------------------------------------------------------------------------------!
subroutine xbin_readend(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) , intent(IN) :: defxbin
! -- private data --
character(len=len(xbin_endsection)) :: str

  read(defxbin%iunit) str
  if (str /= xbin_endsection) &
    call xbin_error("bad end of data section") 

endsubroutine xbin_readend

!------------------------------------------------------------------------------!
subroutine xbin_writeend(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) , intent(IN) :: defxbin
! -- private data --

  write(defxbin%iunit) xbin_endsection

endsubroutine xbin_writeend

!------------------------------------------------------------------------------!
! read and write HEADER
!------------------------------------------------------------------------------!
subroutine xbin_writeheader(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) , intent(IN) :: defxbin
! -- private data --

  write(defxbin%iunit) xbin_strheader
  write(defxbin%iunit) int(xbin_defaultver/256, 1), int(mod(xbin_defaultver, 256), 1)
  write(defxbin%iunit) endian_test

endsubroutine xbin_writeheader

!------------------------------------------------------------------------------!
subroutine xbin_readheader(defxbin)

implicit none
! -- INPUTS --
! -- INPUTS/OUTPUTS --
type(st_defxbin) , intent(INOUT) :: defxbin
! -- private data --
character(len=len(xbin_strheader)) :: strcheck
integer(1)                         :: endiancheck(kendian), ver(2)
integer                            :: info

  read(defxbin%iunit, iostat=info) strcheck
  if (info /=0) call xbin_error("IO error reading XBIN header") 
  if (strcheck /= xbin_strheader) &
    call xbin_error("this file has not a XBIN header") 

  read(defxbin%iunit, iostat=info) ver(1:xbinver)  ! read version number, big endian like
  if (info /=0) call xbin_error("IO error reading XBIN version number") 
  defxbin%xbin_version = big_endian(ver, xbinver)
  if (defxbin%xbin_version > xbin_maxver) &
    call xbin_error("XBIN version number ("//trim(strof(defxbin%xbin_version))//") too high") 

  read(defxbin%iunit, iostat=info) endiancheck(1:kendian)
  if (info /=0) call xbin_error("IO error reading XBIN endianness") 
  defxbin%byteorder = endianness(endian_test, endiancheck(1:kendian))

endsubroutine xbin_readheader

!------------------------------------------------------------------------------!
! open XBIN file (read or write mode)
!------------------------------------------------------------------------------!
subroutine xbin_openread(filename, defxbin)
implicit none
! -- INPUTS --
character(len=*) , intent(IN)  :: filename
! -- OUTPUTS --
type(st_defxbin) , intent(OUT) :: defxbin
! -- private data --
integer           :: info, current_byteorder
character(len=endianstr_len) :: strbyteorder

defxbin%iunit = getnew_io_unit()

open(unit=defxbin%iunit, file=trim(filename), form='unformatted',  &       ! open with supposed NATIVE byte order
                 access='stream', action='read', iostat = info)    ! but will be checked further
if (info /= 0) call xbin_error("cannot open file "//trim(filename))

call xbin_readheader(defxbin)   ! read version and endianness

! -- check endianness --

inquire(unit=defxbin%iunit, convert=strbyteorder)
current_byteorder = ibyteorder(strbyteorder)

if (defxbin%byteorder /= current_byteorder) then
  close(defxbin%iunit)  
  strbyteorder = byteorder_str(defxbin%byteorder)
  call cfd_print("re-open XBIN file with "//trim(strbyteorder))
  open(unit=defxbin%iunit, file=trim(filename), form='unformatted', convert=trim(strbyteorder), &
                 access='stream', action='read', iostat = info)
  if (info /= 0) call xbin_error("cannot open file "//trim(filename))
  call xbin_readheader(defxbin)
endif

endsubroutine xbin_openread

!------------------------------------------------------------------------------!
subroutine xbin_openwrite(filename, defxbin, byteorder)
implicit none
! -- INPUTS --
character(len=*) , intent(IN)            :: filename
integer          , intent(IN) , optional :: byteorder
! -- OUTPUTS --
type(st_defxbin) , intent(OUT)           :: defxbin
! -- private data --
integer :: iunit
integer :: info
character(len=15) :: strbyteorder

if (present(byteorder)) then
  defxbin%byteorder = byteorder
else
  defxbin%byteorder = native_endianness()
endif

defxbin%iunit = getnew_io_unit()

open(unit=defxbin%iunit, file=trim(filename), form='unformatted', access='stream', &
                                      convert=trim(byteorder_str(defxbin%byteorder)), &
                                      action='write', status='replace', iostat = info)
if (info /= 0) call xbin_error("cannot open file "//trim(filename))


call xbin_writeheader(defxbin)

endsubroutine xbin_openwrite

!------------------------------------------------------------------------------!
subroutine xbin_close(defxbin)
implicit none
! -- INPUTS --
! -- OUTPUTS --
type(st_defxbin) :: defxbin
! -- private data --

call close_io_unit(defxbin%iunit)

endsubroutine xbin_close


endmodule XBIN_IO
!------------------------------------------------------------------------------!
! Changes
!
! Mar  2010: created
! Apr  2014: automatic unit affectation
!------------------------------------------------------------------------------!

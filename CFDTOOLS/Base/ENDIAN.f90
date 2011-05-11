!----------------------------------------------------------------------------------------
! MODULE : ENDIAN
! 
! Function
!   Routines to find, check or convert ENDIANNESS or integers and reals
!
!----------------------------------------------------------------------------------------
module ENDIAN

use IOCFD

! -- PUBLIC Variables -------------------------------------------

integer, parameter :: fmt_unknown      = 0
integer, parameter :: fmt_littleendian = 10
integer, parameter :: fmt_bigendian    = 20

! -- interface DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
! gets native endianness of system (internal representation of integers)
!----------------------------------------------------------------------------------------
integer function native_endianness()
implicit none
integer(4) :: i4, little, big
integer(1) :: i1(4)
integer    :: i

i1(1:4) = (/ 10, 20, 30, 40 /)

little = 0
big    = 0
do i = 1, 4
  little = little + i1(i)  *(256**(i-1))
  big    = big    + i1(5-i)*(256**(i-1))
enddo
i4 = transfer(i1, i4)

if (i4 == little) then
  native_endianness = fmt_littleendian
elseif (i4 == big) then
  native_endianness = fmt_bigendian
else
  native_endianness = fmt_unknown
endif

endfunction native_endianness
!
!----------------------------------------------------------------------------------------
! gets endianness of system (internal representation of integers) with an example
!----------------------------------------------------------------------------------------
integer function endianness(i4, i1)
implicit none
integer(4) :: i4, ilittle, ibig
integer(1) :: i1(4)

ilittle = little_endian(i1, size(i1))
ibig    =    big_endian(i1, size(i1))

if (ilittle /= ibig) then
  if (i4 == little_endian(i1, size(i1))) then
    endianness = fmt_littleendian
  elseif (i4 == big_endian(i1, size(i1))) then
    endianness = fmt_bigendian
  else
    endianness = fmt_unknown
  endif
else
  endianness = fmt_unknown
endif

endfunction endianness

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
integer function little_endian(i1, n)
implicit none
integer    :: n
integer(1) :: i1(n)
integer    :: i

little_endian = 0
do i = 1, n
  little_endian = little_endian + i1(i) *(256**(i-1))
enddo

endfunction little_endian

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
integer function big_endian(i1, n)
implicit none
integer    :: n
integer(1) :: i1(n)
integer    :: i

big_endian = 0
do i = 1, n
  big_endian = big_endian + i1(n+1-i) *(256**(i-1))
enddo

endfunction big_endian

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
character(len=15) function byteorder_str(byteorder)
implicit none
integer    :: byteorder

select case(byteorder)
case(fmt_littleendian)
  byteorder_str = 'LITTLE_ENDIAN'
case(fmt_bigendian)
  byteorder_str = 'BIG_ENDIAN'
case(fmt_unknown)
  call cfd_error("byte order was set to 'unknown' (ENDIAN:byteorder_str)")
case default
  call cfd_error("unknown byte order (ENDIAN:byteorder_str)")
endselect

endfunction byteorder_str

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
integer function ibyteorder(strbyteorder)
implicit none
character(len=*) :: strbyteorder

if (strbyteorder == 'LITTLE_ENDIAN') then
  ibyteorder = fmt_littleendian
elseif (strbyteorder == 'BIG_ENDIAN') then
  ibyteorder = fmt_bigendian
elseif (strbyteorder == 'NATIVE') then
  ibyteorder = native_endianness()
elseif (strbyteorder == 'UNKNOWN') then
  ibyteorder = fmt_unknown
else
  call cfd_error("unknown byte order (ENDIAN:ibyteorder)")
endif

endfunction ibyteorder

!----------------------------------------------------------------------------------------
endmodule ENDIAN
!------------------------------------------------------------------------------!
! Changes history
!
! May  2011: creation
!------------------------------------------------------------------------------!

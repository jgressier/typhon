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

integer, parameter :: endian_unknown = 0
integer, parameter :: endian_little  = 10
integer, parameter :: endian_big     = 20

integer, parameter :: endianstr_len     = 15
character(len=endianstr_len), parameter :: endianstr_little  = 'LITTLE_ENDIAN'
character(len=endianstr_len), parameter :: endianstr_big     = 'BIG_ENDIAN'
character(len=endianstr_len), parameter :: endianstr_native  = 'NATIVE'
character(len=endianstr_len), parameter :: endianstr_unknown = 'UNKNOWN'

! -- interface DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
! gets native endianness of system (internal representation of integers)
!----------------------------------------------------------------------------------------
integer function native_endianness()
implicit none
! -- INPUTS --
! -- OUTPUTS --
! -- private data --
integer(4) :: i4, little, big
integer(1) :: i1(4)
integer    :: i

i1(1:4) = (/ 10, 20, 30, 40 /)

little = 0
big    = 0
do i = 1, 4
  little = 256*little + i1(5-i)
  big    = 256*big    + i1(i)
enddo
i4 = transfer(i1, i4)

if (i4 == little) then
  native_endianness = endian_little
elseif (i4 == big) then
  native_endianness = endian_big
else
  native_endianness = endian_unknown
endif

endfunction native_endianness
!
!----------------------------------------------------------------------------------------
! gets endianness of system (internal representation of integers) with an example
!----------------------------------------------------------------------------------------
integer function endianness(i4, i1)
implicit none
! -- INPUTS --
integer(4) , intent(IN) :: i4
integer(1) , intent(IN) :: i1(4)
! -- OUTPUTS --
! -- private data --
integer(4) :: ilittle, ibig

ilittle = little_endian(i1, size(i1))
ibig    =    big_endian(i1, size(i1))

if (ilittle /= ibig) then
  if (i4 == little_endian(i1, size(i1))) then
    endianness = endian_little
  elseif (i4 == big_endian(i1, size(i1))) then
    endianness = endian_big
  else
    endianness = endian_unknown
  endif
else
  endianness = endian_unknown
endif

endfunction endianness

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
integer function little_endian(i1, n)
implicit none
! -- INPUTS --
integer    :: n
integer(1) :: i1(n)
! -- OUTPUTS --
! -- private data --
integer    :: i

little_endian = 0
do i = n, 1, -1
  little_endian = 256*little_endian + i1(i)
enddo

endfunction little_endian

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
integer function big_endian(i1, n)
implicit none
! -- INPUTS --
integer    :: n
integer(1) :: i1(n)
! -- OUTPUTS --
! -- private data --
integer    :: i

big_endian = 0
do i = 1, n
  big_endian = 256*big_endian + i1(i)
enddo

endfunction big_endian

!----------------------------------------------------------------------------------------
! convert bytes to little endian integer*n
!----------------------------------------------------------------------------------------
character(len=endianstr_len) function byteorder_str(byteorder)
implicit none
! -- INPUTS --
integer , intent(IN) :: byteorder
! -- OUTPUTS --
! -- private data --

select case(byteorder)
case(endian_little)
  byteorder_str = endianstr_little
case(endian_big)
  byteorder_str = endianstr_big
case(endian_unknown)
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
! -- INPUTS --
character(len=*) :: strbyteorder
! -- OUTPUTS --
! -- private data --

select case(strbyteorder)
case(endianstr_little)
  ibyteorder = endian_little
case(endianstr_big)
  ibyteorder = endian_big
case(endianstr_native)
  ibyteorder = native_endianness()
case(endianstr_unknown)
  ibyteorder = endian_unknown
case default
  call cfd_error("unknown byte order (ENDIAN:ibyteorder)")
endselect

endfunction ibyteorder

!----------------------------------------------------------------------------------------
endmodule ENDIAN
!------------------------------------------------------------------------------!
! Changes history
!
! May  2011: creation
!------------------------------------------------------------------------------!

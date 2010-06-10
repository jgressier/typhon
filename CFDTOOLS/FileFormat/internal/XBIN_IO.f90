!------------------------------------------------------------------------------!
! MODULE : XBIN_IO
!
!------------------------------------------------------------------------------!
module XBIN_IO

use IOCFD

implicit none

! -- Global Variables -------------------------------------------

integer,           parameter :: xbinkpp = 2
integer,           parameter :: xbinkrp = 8
integer,           parameter :: xbinkip = 4
integer(xbinkpp),  parameter :: endian_test     = 1
integer(xbinkpp),  parameter :: xbin_defaultver = 1
integer(xbinkpp),  parameter :: xbin_maxver     = 1
character(len=4),  parameter :: xbin_strheader  = "XBIN"
character(len=11), parameter :: xbin_prefix     = "[XBIN lib] "
character(len=3),  parameter :: xbin_endsection = "END"

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Structures
!------------------------------------------------------------------------------!
type st_defxbin
  integer          :: iunit
  logical          :: bigendian
  integer(xbinkpp) :: xbin_version
endtype st_defxbin

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! test end of XBIN file
!------------------------------------------------------------------------------!
logical function xbin_eof(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) :: defxbin
! -- private data --
integer info

  xbin_eof = eof(defxbin%iunit)
  !inquire(defxbin%iunit, iostat=info)

endfunction xbin_eof


!------------------------------------------------------------------------------!
! read and write END of section
!------------------------------------------------------------------------------!
subroutine xbin_readend(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) :: defxbin
! -- private data --
character(len=len(xbin_endsection)) :: str

  read(defxbin%iunit) str
  if (str /= xbin_endsection) &
    call cfd_error(xbin_prefix//" bad end of data section") 

endsubroutine xbin_readend

!------------------------------------------------------------------------------!
subroutine xbin_writeend(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) :: defxbin
! -- private data --

  write(defxbin%iunit) xbin_endsection

endsubroutine xbin_writeend

!------------------------------------------------------------------------------!
! read and write HEADER
!------------------------------------------------------------------------------!
subroutine xbin_writeheader(defxbin)
implicit none
! -- INPUTS --
type(st_defxbin) :: defxbin
! -- private data --

  write(defxbin%iunit) xbin_strheader
  write(defxbin%iunit) endian_test
  write(defxbin%iunit) xbin_defaultver

endsubroutine xbin_writeheader

!------------------------------------------------------------------------------!
subroutine xbin_readheader(defxbin)

implicit none
! -- INPUTS --
! -- INPUTS/OUTPUTS --
type(st_defxbin) :: defxbin
! -- private data --
character(len=len(xbin_strheader)) :: strcheck
integer(xbinkpp)                   :: endiancheck

  read(defxbin%iunit) strcheck
  if (strcheck /= xbin_strheader) &
     call cfd_error(xbin_prefix//" this file has not a XBIN header") 

  read(defxbin%iunit) endiancheck
  if (endiancheck /= endian_test) &
     call cfd_error(xbin_prefix//" this file has unexpected endianness") 

  read(defxbin%iunit) defxbin%xbin_version

endsubroutine xbin_readheader

!------------------------------------------------------------------------------!
! open XBIN file (read or write mode)
!------------------------------------------------------------------------------!
subroutine xbin_openread(iunit, filename, defxbin)
implicit none
! -- INPUTS --
integer          :: iunit
character(len=*) :: filename
! -- OUTPUTS --
type(st_defxbin) :: defxbin
! -- private data --
integer :: info

  open(unit=iunit, file=trim(filename), form='unformatted', access='stream', iostat = info)
  if (info /= 0) call cfd_error(xbin_prefix//"cannot open file "//trim(filename))

  defxbin%iunit = iunit

  call xbin_readheader(defxbin)

endsubroutine xbin_openread

!------------------------------------------------------------------------------!
subroutine xbin_openwrite(iunit, filename, defxbin)
implicit none
! -- INPUTS --
integer          :: iunit
character(len=*) :: filename
! -- OUTPUTS --
type(st_defxbin) :: defxbin
! -- private data --
integer :: info

  open(unit=iunit, file=trim(filename), form='unformatted', access='stream', iostat = info)
  if (info /= 0) call cfd_error(xbin_prefix//"cannot open file "//trim(filename))

  defxbin%iunit = iunit

  call xbin_writeheader(defxbin)

endsubroutine xbin_openwrite




endmodule XBIN_IO
!------------------------------------------------------------------------------!
! Changes
!
! Mar  2010: 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! MODULE : IO_UNIT
!
! Manage unit numbers for IO
!------------------------------------------------------------------------------!
module IO_UNIT

implicit none

! -- PUBLIC DATA -------------------------------------------------------------

! -- PRIVATE DATA -------------------------------------------------------------

integer, private, parameter :: io_unit_min = 100
integer, private, parameter :: io_unit_max = 500

logical, private, dimension(io_unit_min:io_unit_max) :: io_unit_opened = .false.

! -- INTERFACES -------------------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! getnew_io_unit: get first unused unit, -1 if not found
!------------------------------------------------------------------------------!
integer function getnew_io_unit()
implicit none
! -- INPUTS --
! -- Internal variables --
integer :: i
! -- BODY --

getnew_io_unit   = -1
do i = io_unit_min, io_unit_max
  if (.not.io_unit_opened(i)) then
    io_unit_opened(i) = .true.
    getnew_io_unit   = i
    exit
  endif
enddo

endfunction getnew_io_unit

!------------------------------------------------------------------------------!
! close_io_unit
!------------------------------------------------------------------------------!
subroutine close_io_unit(iounit)
implicit none
! -- INPUTS --
integer :: iounit
! -- Internal variables --
! -- BODY --

close(iounit)
io_unit_opened(iounit) = .false.

endsubroutine close_io_unit

!------------------------------------------------------------------------------!
! test end of XBIN file
!------------------------------------------------------------------------------!
logical function io_eof(iounit)
implicit none
! -- INPUTS --
integer :: iounit
! -- Internal variables --
#ifndef __INTEL_COMPILER
integer info
#endif /*__INTEL_COMPILER*/
! -- BODY --

#ifdef __INTEL_COMPILER
  io_eof = eof(iounit)  ! intrinsic INTEL fortran
#else  /*NO __INTEL_COMPILER*/
  inquire(iounit, iostat=info)   ! fortran norm but does not work
  io_eof = is_iostat_end(info)
#endif /*__INTEL_COMPILER*/

endfunction io_eof

endmodule IO_UNIT
!------------------------------------------------------------------------------!
! Change history
!
! Jun 2009 : created
!------------------------------------------------------------------------------!

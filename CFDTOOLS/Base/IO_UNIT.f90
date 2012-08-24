!------------------------------------------------------------------------------!
! MODULE : IO_UNIT
!
! Manage unit numbers for IO
!------------------------------------------------------------------------------!
module IO_UNIT

implicit none

! -- PUBLIC DATA -------------------------------------------------------------

!integer :: getnew_io_unit

! -- PRIVATE DATA -------------------------------------------------------------

integer, private, parameter ::io_unit_min = 100
integer, private, parameter ::io_unit_max = 500

logical, private, dimension(io_unit_min:io_unit_max) :: io_unit_state = .false.

! -- INTERFACES -------------------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! getnew_io_unit: get first unused unit, -1 if not found
!------------------------------------------------------------------------------!

integer function getnew_io_unit()
implicit none
integer :: i 

i = io_unit_min
do while (i <= io_unit_max)
  if (.not.io_unit_state(i)) exit   ! if not used
  i = i + 1
enddo
if (i <= io_unit_max) then
  io_unit_state(i) = .true.
  getnew_io_unit   = i
else
  getnew_io_unit   = -1
endif

endfunction

!------------------------------------------------------------------------------!
! set_closed_io_unit
!------------------------------------------------------------------------------!
subroutine set_closed_io_unit(iounit)
implicit none
integer :: iounit

io_unit_state(iounit) = .false.

endsubroutine set_closed_io_unit

!------------------------------------------------------------------------------!
! close_io_unit
!------------------------------------------------------------------------------!
subroutine close_io_unit(iounit)
implicit none
integer :: iounit

close(iounit)
call set_closed_io_unit(iounit)

endsubroutine close_io_unit


endmodule IO_UNIT
!------------------------------------------------------------------------------!
! Change history
!
! June 2009: created
!------------------------------------------------------------------------------!

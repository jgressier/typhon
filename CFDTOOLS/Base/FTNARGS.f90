!------------------------------------------------------------------------------!
! MODULE : FTNARGS
! 
! Lecture of command line arguments with optional increment of the arg count
!------------------------------------------------------------------------------!
module FTNARGS

use TYPHMAKE   !> @todo must generalize FTNARGS functions to many kinds
use STRING

! -- DECLARATIONS -----------------------------------------------------------

! -- Constants    -----------------------------------------------------------

! -- INTERFACES -------------------------------------------------------------

interface read_command_argument
  module procedure read_command_argument_real, &
                   read_command_argument_integer, &
                   read_command_argument_string
endinterface

!interface operator(+)
!interface operator(.vect.)

! -- Functions and Operators ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Function : read command argument of type real
!            (and optionally increment the argument count,
!                            set the error status,
!                        and return the string read)
!------------------------------------------------------------------------------!
subroutine read_command_argument_real(iarg, var_real, lincr, ierr, opt_string)
!------------------------------------------------------------------------------!
implicit none
integer(kip), intent(inout) :: iarg
real(krp),    intent(out)   :: var_real
logical,      intent(in),  optional :: lincr
integer(kip), intent(out), optional :: ierr
character(len=*), intent(out), optional :: opt_string
!------------------------------------------------------------------------------!
character(len=256) :: str_read

call get_command_argument(iarg, str_read)
if (present(lincr) .AND. lincr) then
  iarg = iarg+1
endif
if (is_real(str_read)) then
  if (present(ierr)) then
    read(str_read,*,iostat=ierr) var_real
  else
    read(str_read,*) var_real
  endif
else
  if (present(ierr)) ierr = 1
endif
if (present(opt_string)) opt_string = str_read

endsubroutine read_command_argument_real

!------------------------------------------------------------------------------!
! Function : read command argument of type integer
!            (and optionally increment the argument count,
!                            set the error status,
!                        and return the string read)
!------------------------------------------------------------------------------!
subroutine read_command_argument_integer(iarg, var_integer, lincr, ierr, opt_string)
!------------------------------------------------------------------------------!
implicit none
integer(kip), intent(inout) :: iarg
integer(kip), intent(out)   :: var_integer
logical,      intent(in),  optional :: lincr
integer(kip), intent(out), optional :: ierr
character(len=*), intent(out), optional :: opt_string
!------------------------------------------------------------------------------!
character(len=256) :: str_read

call get_command_argument(iarg, str_read)
if (present(lincr) .AND. lincr) then
  iarg = iarg+1
endif
if (is_int(str_read)) then
  if (present(ierr)) then
    read(str_read,*,iostat=ierr) var_integer
  else
    read(str_read,*) var_integer
  endif
else
  if (present(ierr)) ierr = 1
endif
if (present(opt_string)) opt_string = str_read

endsubroutine read_command_argument_integer

!------------------------------------------------------------------------------!
! Function : read command argument of type string
!            (and optionally increment the argument count)
!------------------------------------------------------------------------------!
subroutine read_command_argument_string(iarg, var_string, lincr)
!------------------------------------------------------------------------------!
implicit none
integer(kip),     intent(inout) :: iarg
character(len=*), intent(out)   :: var_string
logical,          intent(in), optional :: lincr
!------------------------------------------------------------------------------!

call get_command_argument(iarg, var_string)
if (present(lincr) .AND. lincr) then
  iarg = iarg+1
endif

endsubroutine read_command_argument_string



endmodule FTNARGS
!------------------------------------------------------------------------------!
! Changes history
!
! jan 2012 : module creation
!------------------------------------------------------------------------------!


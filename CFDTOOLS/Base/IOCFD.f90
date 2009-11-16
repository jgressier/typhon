!------------------------------------------------------------------------------!
! MODULE : IOCFD
!------------------------------------------------------------------------------!
module IOCFD

implicit none

! -- Module DATA ------------------------------------------------------------

integer :: print_unit = 6

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : error_stop
! Fonction  : write error and stop executation
!------------------------------------------------------------------------------!
subroutine cfd_error(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,*) "Error in CFDTOOLS libraries: ",trim(str)
stop 1

endsubroutine cfd_error

!------------------------------------------------------------------------------!
! Procedure : cfd_print
! Fonction  : print information from CFDtools libraries
!------------------------------------------------------------------------------!
subroutine cfd_print(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,'(a,a)') "[cfdtools] ",trim(str)

endsubroutine cfd_print

!------------------------------------------------------------------------------!
! Procedure : cfd_warning
! Fonction  : print warning and do not stop
!------------------------------------------------------------------------------!
subroutine cfd_warning(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,*) "Warning in CFDTOOLS libraries: ",trim(str)

endsubroutine cfd_warning



endmodule IOCFD
!------------------------------------------------------------------------------!
! History
! Oct  2009: created, error and warning outputs
!------------------------------------------------------------------------------!

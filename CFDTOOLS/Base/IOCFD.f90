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

write(print_unit,'(2a)') "Error in CFDTOOLS libraries: ",trim(str)
stop 1

endsubroutine cfd_error

!------------------------------------------------------------------------------!
! Procedure : cfd_warning
! Fonction  : print warning and do not stop
!------------------------------------------------------------------------------!
subroutine cfd_warning(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,'(2a)') "Warning in CFDTOOLS libraries: ",trim(str)

endsubroutine cfd_warning

!------------------------------------------------------------------------------!
! Procedure : cfd_print
! Fonction  : print information from CFDtools libraries
!------------------------------------------------------------------------------!
subroutine cfd_print(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,'(2a)') "[cfdtools] ",trim(str)

endsubroutine cfd_print

!------------------------------------------------------------------------------!
! Procedure : cfd_write
! Fonction  : print information
!------------------------------------------------------------------------------!
subroutine cfd_write(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,'(1a)') trim(str)

endsubroutine cfd_write

!------------------------------------------------------------------------------!
! Procedure : print_cfdtools_header
! Fonction  : print information from CFDtools libraries
!------------------------------------------------------------------------------!
subroutine print_cfdtools_header(str)
implicit none
! -- INPUTS --
character(len=*):: str
! -- BODY --

write(print_unit,'(1a)') repeat('-',40)
write(print_unit,'(1a)') trim(str)
write(print_unit,'(1a)') repeat('-',40)

endsubroutine print_cfdtools_header



endmodule IOCFD
!------------------------------------------------------------------------------!
! History
! Oct  2009: created, error and warning outputs
!------------------------------------------------------------------------------!

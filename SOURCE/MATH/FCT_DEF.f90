!------------------------------------------------------------------------------!
! MODULE : FCT_DEF                        Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (see history)
!   Definition of NODE DEF
!
!------------------------------------------------------------------------------!

module FCT_DEF

implicit none

! -- Constants -------------------------------------------

integer, parameter :: rprc = 8      ! internal real precision
integer, parameter :: iprc = 8      ! internal integer precision
integer, parameter :: ipar = 2      ! internal integer precision for parameters

! -- variables  -------------------------------------------

logical,       private :: fct_stop
integer(ipar), private :: fct_error

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! set_fct_error
!------------------------------------------------------------------------------!
subroutine set_fct_error(type, comment)
implicit none
! - parameters
integer           :: type
character(len=*)  :: comment

  print*,"FCT MODULE error : ",type,comment
  stop

endsubroutine set_fct_error

!------------------------------------------------------------------------------!
! get_fct_error
!------------------------------------------------------------------------------!
integer(ipar) function get_fct_error()
implicit none

  get_fct_error = fct_error

end function get_fct_error


endmodule FCT_DEF
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : module creation
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! MODULE : FCT_DEF                        Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (see history)
!   Accuracy definition
!   Error handlers
!
!------------------------------------------------------------------------------!

module FCT_DEF

implicit none

! -- Constants -------------------------------------------

integer, parameter :: rprc = 8      ! internal real precision
integer, parameter :: iprc = 8      ! internal integer precision
integer, parameter :: ipar = 2      ! internal integer precision for parameters

! -- variables -------------------------------------------

logical, private :: fct_stop
integer, private :: fct_error

! -- interfaces ------------------------------------------

interface set_fct_error
  module procedure set_fct_error_intipar, set_fct_error_int
endinterface

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! set_fct_error
!------------------------------------------------------------------------------!
subroutine set_fct_error_intipar(type, comment)
implicit none
! - parameters
integer(ipar)     :: type
character(len=*)  :: comment

  print*,"FCT MODULE error : ",type,comment
  stop

endsubroutine set_fct_error_intipar

!-------------------------------------------------
subroutine set_fct_error_int(type, comment)
implicit none
! - parameters
integer           :: type
character(len=*)  :: comment

  print*,"FCT MODULE error : ",type,comment
  stop

endsubroutine set_fct_error_int

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

!------------------------------------------------------------------------------!
! MODULE : FCT_CONTAINER                  Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (see history)
!   Definition of NODE CONTAINER
!
!------------------------------------------------------------------------------!

module FCT_CONTAINER

use FCT_DEF     ! accuracy definition and error handlers
use GEO3D       ! 3D vector type

implicit none

! -- Constants -------------------------------------------

integer(ipar), parameter :: cont_str     = 20

! -- node container type

integer(ipar), parameter :: cont_real    = 10
integer(ipar), parameter :: cont_vect    = 15
integer(ipar), parameter :: cont_mat     = 30
integer(ipar), parameter :: cont_v3d     = 50
integer(ipar), parameter :: cont_v3dvect = 55
integer(ipar), parameter :: cont_func    = 100

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_CONTAINER : 
!------------------------------------------------------------------------------!
type st_fct_container
  integer(ipar)   :: type
  integer(iprc)   :: size
  character(len=cont_str) :: name
  real(rprc)              :: r
  real(rprc), pointer     :: r_t(:)
  type(v3d)               :: v3d
endtype st_fct_container


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_fct_container
endinterface

interface delete
  module procedure delete_fct_container
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_container : allocate FCT_CONTAINER structure
!------------------------------------------------------------------------------!
subroutine new_fct_container(container, type, n)
implicit none
! - parameters
type(st_fct_container)  :: container
integer(ipar)           :: type
integer(iprc), optional :: n

  container%type = type

  select case(type)
  case(cont_real)
  case(cont_vect)
    if (present(n)) then
      container%size = n
      allocate(container%r_t(n))
    else
      call set_fct_error(1,"bad definition of vector node")
    endif
  case(cont_v3d)
  case default
    call set_fct_error(1,"unknown type of node container")
  endselect

endsubroutine new_fct_container

!------------------------------------------------------------------------------!
! delete_fct_container : remove FCT_CONTAINER structure
!------------------------------------------------------------------------------!
subroutine delete_fct_container(container)
implicit none
! - paramètres
type(st_fct_container) :: container

  select case(container%type)
  case(cont_real)
  case(cont_vect)
    deallocate(container%r_t)
  case(cont_v3d)
  case default
    call set_fct_error(1,"unknown type of node")
  endselect

endsubroutine delete_fct_container


endmodule FCT_CONTAINER


!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : module creation
!------------------------------------------------------------------------------!

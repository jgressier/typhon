!------------------------------------------------------------------------------!
! MODULE : FCT_CONTAINER                  Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (see history)
!   Definition of NODE CONTAINER
!   should represent any kind of variable
!
!------------------------------------------------------------------------------!

module FCT_CONTAINER

use FCT_DEF     ! accuracy definition and error handlers
use GEO3D       ! 3D vector type

implicit none

! -- Constants -------------------------------------------

integer, parameter       :: cont_str     = 20

! -- node container type

integer(ipar), parameter :: cont_var      = 01           ! variable
integer(ipar), parameter :: cont_real     = 10           ! real
integer(ipar), parameter :: cont_vect     = 15           ! array of reals
integer(ipar), parameter :: cont_mat      = 30           ! matrix
integer(ipar), parameter :: cont_v3d      = 50           ! 3D vector
integer(ipar), parameter :: cont_v3dvect  = 55           ! array of 3D vectors

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_CONTAINER : polymorphic mathematical element
!------------------------------------------------------------------------------!
type st_fct_container
  integer(ipar)   :: type               ! type of the element
  integer(iprc)   :: size               ! size of element (if variable size)
  character(len=cont_str) :: name       ! name of element itself or external variable
  real(rprc)              :: r          ! if element is real
  real(rprc), pointer     :: r_t(:)     ! if element is a vector(size)
  type(v3d)               :: v3d        ! if element is a 3D vector (x,y,z)
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
      call set_fct_error(1_ipar, "bad definition of vector node")
    endif
  case(cont_v3d)
  case(cont_var)
  case default
    call set_fct_error(1_ipar, "unknown type of node container")
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
    call set_fct_error(1_ipar, "unknown type of node")
  endselect

endsubroutine delete_fct_container


endmodule FCT_CONTAINER


!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : module creation
!------------------------------------------------------------------------------!

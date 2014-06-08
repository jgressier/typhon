!------------------------------------------------------------------------------!
! MODULE : FCT_CONTAINER                  Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (see history)
!   Definition of NODE CONTAINER
!   should represent any kind of variable
!
!------------------------------------------------------------------------------!
!> @ingroup FCT
!> @brief meta container for FCT node
!> able to represent a variable, a real, or an array of reals
!------------------------------------------------------------------------------!
module FCT_CONTAINER

use FCT_DEF     ! accuracy definition and error handlers
use VEC3D       ! 3D vector type

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
subroutine new_fct_container(container, type, name, n)
implicit none
! - parameters
type(st_fct_container), intent(out) :: container
integer(ipar),          intent(in)  :: type
character(len=*),       intent(in)  :: name
integer(iprc), optional :: n

  container%type = type
  container%name = name
  container%size = 0

  select case(type)
  case(cont_real)
    ! nothing else to do
  case(cont_vect)
    if (present(n)) then
      container%size = n
      allocate(container%r_t(n))
    else
      call set_fct_error(1_ipar, "bad definition of vector node: size expected")
    endif
  case(cont_v3d)
    ! nothing else to do
  case(cont_var)
    if (len_trim(name) == 0) &
      call set_fct_error(-1, "bad definition of variable node: name expected")
  case default
    call set_fct_error(1_ipar, "unknown type of node container (in new_fct_container)")
  endselect

endsubroutine new_fct_container

!------------------------------------------------------------------------------!
! copy_fct_container : allocate FCT_CONTAINER structure
!------------------------------------------------------------------------------!
subroutine copy_fct_container(source, dest)
implicit none
! - parameters
type(st_fct_container), intent(in)  :: source
type(st_fct_container), intent(out) :: dest

  dest%type = source%type

  select case(source%type)
  case(cont_real)
    dest%r    = source%r
    dest%size = source%size  ! must be 0
  case(cont_vect)
    dest%size = source%size
    allocate(dest%r_t(dest%size))
    dest%r_t(1:dest%size) = source%r_t(1:source%size) 
  case(cont_v3d)
    dest%v3d = source%v3d
  case(cont_var)
    dest%name = source%name
  case default
    call set_fct_error(1_ipar, "unknown type of node container (in copy_fct_container)")
  endselect

endsubroutine copy_fct_container

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
  case(cont_var)
  case default
    call set_fct_error(1_ipar, "unknown type of node container (in delete_fct_container)")
  endselect

endsubroutine delete_fct_container


endmodule FCT_CONTAINER


!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : module creation
!------------------------------------------------------------------------------!

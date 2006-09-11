!------------------------------------------------------------------------------!
! MODULE : FCT_ENV                          Authors : J. Gressier
!                                           Date    : July 2006
! Module definition aimed 
!
!------------------------------------------------------------------------------!

module FCT_ENV

use FCT_DEF           ! basic definition
use FCT_CONTAINER     ! generalized variable
use FCT_NODE          ! node structure for chained list

implicit none

! -- Constants -------------------------------------------



     
! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_ENV : 
!------------------------------------------------------------------------------!
type st_fct_env
  logical                    :: uptodate
  integer                    :: nvar
  type(st_fct_node), pointer :: var_stack
endtype st_fct_env


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_env : allocate FCT_ENV structure
!------------------------------------------------------------------------------!
subroutine new_fct_env(env)
implicit none
! - parameters
type(st_fct_env)  :: env

  ! -- initialize ENV to no constant/function --
  env%uptodate = .false.
  env%nvar     = 0
  if (associated(env%var_stack)) call delete_fct_env(env)
  nullify(env%var_stack)  

endsubroutine new_fct_env

!------------------------------------------------------------------------------!
! delete_fct_env : deallocate FCT_ENV structure
!------------------------------------------------------------------------------!
subroutine delete_fct_env(env)
implicit none
! - parameters
type(st_fct_env)           :: env
type(st_fct_node), pointer :: p
integer                    :: i

  if (env%nvar > 0) then
    do i = 1, env%nvar
      p => env%var_stack%right
      call delete_fct_node(env%var_stack)
      env%var_stack => p
    enddo
    if (associated(p)) call fct_warning("bad number of variables while FCT_ENV deleting")
  endif

endsubroutine delete_fct_env

!------------------------------------------------------------------------------!
! fct_env_seek_name : obtain node pointer of a name in ENVironment
!------------------------------------------------------------------------------!
subroutine fct_env_seek_name(env, name, p)
implicit none
! - parameters
type(st_fct_env)           :: env
character(len=*)           :: name
type(st_fct_node), pointer :: p

  p => env%var_stack
  do while (associated(p))
    if (samestring(name, p%container%name)) exit
    p => p%right
  enddo

endsubroutine fct_env_seek_name

!------------------------------------------------------------------------------!
! fct_env_set_real
!------------------------------------------------------------------------------!
subroutine fct_env_set_real(env, name, x)
implicit none
! - parameters
type(st_fct_env), intent(out) :: env
character(len=*), intent(in)  :: name
real(rprc),       intent(in)  :: x
type(st_fct_node), pointer :: p

  env%uptodate = .false.              ! environment has been changed

  call fct_env_seek_name(env, name, p)

  if (.not.associated(p)) then        ! variable NAME does not exist : must create it

    ! -- create constant --
    allocate(p)
    call new_fct_node(p, node_cst, lowercase(name))    ! create node and container and assign name

    ! --  add element to stack --
    env%nvar      =  env%nvar + 1
    p%right       => env%var_stack
    env%var_stack => p

  endif

  p%container%r    = x              ! assign value 

endsubroutine fct_env_set_real


!------------------------------------------------------------------------------!
! print_fct_env
!------------------------------------------------------------------------------!
subroutine print_fct_env(unit, env)
implicit none
! - parameters
integer                             :: unit
type(st_fct_env),        intent(in) :: env
type(st_fct_node), pointer :: p

  p => env%var_stack

  do while (associated(p))

    select case(p%container%type)
    case(cont_real)
      print*,p%container%name,":",p%container%r
    case default
      call set_fct_error(-1, "incorrect or non-implemented type in FCT Environment")
    endselect

    p => p%right

  enddo

  endsubroutine print_fct_env


!------------------------------------------------------------------------------!
endmodule FCT_ENV
!------------------------------------------------------------------------------!
! Changes history
!
! July 2006 : module creation, environment with real constants
!------------------------------------------------------------------------------!

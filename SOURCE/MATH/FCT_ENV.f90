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
  env%nvar = 0
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
    if (associated(p)) call set_fct_error(-1, "bad number of variables while FCT_ENV deleting")
  endif

endsubroutine delete_fct_env

!------------------------------------------------------------------------------!
! fct_env_add_real
!------------------------------------------------------------------------------!
subroutine fct_env_add_real(env, name, x)
implicit none
! - parameters
type(st_fct_env)           :: env
character(len=cont_str)    :: name
real(rprc)                 :: x
type(st_fct_node), pointer :: p

  ! -- create constant --
  allocate(p)
  call new_fct_node(p, node_cst)    ! create node and container
  p%container%r    = x              ! assign value and name
  p%container%name = name

  ! --  add element to stack --
  env%nvar     =  env%nvar + 1
  p%right      => env%var_stack
  env%var_stack => p

endsubroutine fct_env_add_real


!------------------------------------------------------------------------------!
endmodule FCT_ENV
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2006 : module creation
!------------------------------------------------------------------------------!

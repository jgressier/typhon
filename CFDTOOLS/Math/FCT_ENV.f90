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
  !if (associated(env%var_stack)) call delete_fct_env(env)
  env%uptodate = .false.
  env%nvar     = 0
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
      deallocate(env%var_stack)
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
character(len=cont_str)    :: lname
type(st_fct_node), pointer :: p

  p => env%var_stack
  lname = lowercase(name)
  do while (associated(p))
    if (lname == p%container%name) exit
    p => p%right
  enddo

endsubroutine fct_env_seek_name

!------------------------------------------------------------------------------!
! fct_env_set_container
!------------------------------------------------------------------------------!
subroutine fct_env_set_container(env, name, cont)
implicit none
! - parameters
type(st_fct_env),       intent(out) :: env
character(len=*),       intent(in)  :: name
type(st_fct_container), intent(in)  :: cont
type(st_fct_node),      pointer     :: p

  env%uptodate = .false.              ! environment has been changed

  call fct_env_seek_name(env, name, p)

  if (.not.associated(p)) then        ! variable NAME does not exist : must create it

    ! -- create constant --
    allocate(p)
    call new_fct_node_cst(p, lowercase(name))    ! create node and container and assign name

    ! --  add element to stack --
    env%nvar      =  env%nvar + 1
    p%right       => env%var_stack
    env%var_stack => p

  else
    if (p%container%size /= 0) deallocate(p%container%r_t)
  endif

  p%container      = cont              ! assign value ! DEV: lost of previous r_t pointer ?
  p%container%name = lowercase(name)

endsubroutine fct_env_set_container

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
    call new_fct_node_cst(p, lowercase(name))    ! create node and container and assign name

    ! --  add element to stack --
    env%nvar      =  env%nvar + 1
    p%right       => env%var_stack
    env%var_stack => p

  endif

  p%container%r    = x              ! assign value 

endsubroutine fct_env_set_real

!------------------------------------------------------------------------------!
! fct_env_set_realarray
!------------------------------------------------------------------------------!
subroutine fct_env_set_realarray(env, name, x)
implicit none
! - parameters
type(st_fct_env), intent(out) :: env
character(len=*), intent(in)  :: name
real(rprc),       intent(in)  :: x(:)
type(st_fct_node), pointer :: p
integer(iprc)              :: s

  env%uptodate = .false.              ! environment has been changed
  s = size(x,1)

  call fct_env_seek_name(env, name, p)

  if (.not.associated(p)) then        ! variable NAME does not exist : must create it

    ! -- create constant --
    allocate(p)
    call new_fct_node_cst(p, lowercase(name), s)    ! create node and container and assign name

    ! --  add element to stack --
    env%nvar      =  env%nvar + 1
    p%right       => env%var_stack
    env%var_stack => p
    
  endif

  p%container%r_t(1:s) = x(1:s)              ! assign value 

endsubroutine fct_env_set_realarray


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
    case(cont_vect)
      print*,p%container%name,":",p%container%size
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

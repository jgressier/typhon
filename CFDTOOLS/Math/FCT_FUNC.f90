!------------------------------------------------------------------------------!
! MODULE : FCT_FUNC                       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Definition of FUNCTION
!
!------------------------------------------------------------------------------!

module FCT_FUNC

use FCT_CONTAINER
use FCT_NODE
use FCT_ENV
use FCT_EVAL

implicit none


! -- Constants -------------------------------------------


     
! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_FUNC : 
!------------------------------------------------------------------------------!
type st_fct_func
  character(len=cont_str) :: name
  type(st_fct_node)       :: func
  logical                 :: needed
endtype st_fct_func

!------------------------------------------------------------------------------!
! structure ST_FCTFUNCSET : 
!------------------------------------------------------------------------------!
type st_fctfuncset
  integer(kip)               :: nfct      ! number of functions
  type(st_fct_func), pointer :: fct(:)    ! environnement functions
endtype st_fctfuncset



! -- INTERFACES -------------------------------------------------------------
!interface new
!  module procedure new_fct_func
!endinterface

!interface delete
!  module procedure delete_fct_func
!endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_func : allocate FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine new_fct_func(f)
implicit none
! -- parameters --
type(st_fct_func) :: f


endsubroutine new_fct_func


!------------------------------------------------------------------------------!
! delete_fct_func : remove FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine delete_fct_func(f)
implicit none
! - parameters
type(st_fct_func) :: f

  call delete_fct_node(f%func)
endsubroutine delete_fct_func

!------------------------------------------------------------------------------!
! new_fctfuncset : allocate FCT_FUNCSET structure
!------------------------------------------------------------------------------!
subroutine new_fctfuncset(fset, n)
implicit none
! -- parameters --
type(st_fctfuncset) :: fset
integer, optional   :: n

fset%nfct = 0
if (present(n)) fset%nfct = n
if (fset%nfct == 0) then
  nullify(fset%fct)
else
  allocate(fset%fct(fset%nfct))
endif

endsubroutine

!------------------------------------------------------------------------------!
! delete_fct_func : remove FCTFUNCSET structure
!------------------------------------------------------------------------------!
subroutine delete_fctfuncset(fctenv)
implicit none
! - parameters
type(st_fctfuncset) :: fctenv
integer             :: i

  if (fctenv%nfct >= 1) then
    do i = 1, fctenv%nfct
      call delete_fct_node(fctenv%fct(i)%func)
    enddo
    deallocate(fctenv%fct)
  endif  

endsubroutine 

!------------------------------------------------------------------------------!
! Routine : compute environment
!------------------------------------------------------------------------------!
subroutine fctset_compute_allenv(fctenv, varenv)
implicit none
type(st_fctfuncset) :: fctenv
type(st_fct_env)    :: varenv
integer          :: i
real(krp)        :: x

do i = 1, fctenv%nfct
  call fct_eval_real(   varenv, fctenv%fct(i)%func, x)
  call fct_env_set_real(varenv, fctenv%fct(i)%name, x)
enddo

endsubroutine

!------------------------------------------------------------------------------!
! Routine : compute environment
!------------------------------------------------------------------------------!
subroutine fctset_compute_neededenv(fctenv, varenv)
implicit none
type(st_fctfuncset)        :: fctenv
type(st_fct_env)           :: varenv
type(st_fct_container)     :: res
type(st_fct_node), pointer :: p
integer          :: i
real(krp)        :: x

do i = 1, fctenv%nfct
  if (fctenv%fct(i)%needed) then
    call fct_node_eval(varenv, fctenv%fct(i)%func, res)
    call fct_env_set_container(varenv, fctenv%fct(i)%name, res)  
    ! no delete of res since it is linked to environment
  endif  
enddo

endsubroutine

!------------------------------------------------------------------------------!
! Routine : check dependency
!------------------------------------------------------------------------------!
function fctset_all_dependency(fctenv, name) result(depend)
implicit none
logical             :: depend
type(st_fctfuncset) :: fctenv
character(len=*)    :: name
integer             :: i

depend = .false.
do i = 1, fctenv%nfct
  depend = depend .or. fct_dependency(fctenv%fct(i)%func, name)
enddo

endfunction

!------------------------------------------------------------------------------!
! Routine : check dependency
!------------------------------------------------------------------------------!
function fctset_needed_dependency(fctenv, name) result(depend)
implicit none
logical             :: depend
type(st_fctfuncset) :: fctenv
character(len=*)    :: name
integer             :: i

depend = .false.
do i = 1, fctenv%nfct
  if (fctenv%fct(i)%needed) depend = depend .or. fct_dependency(fctenv%fct(i)%func, name)
enddo

endfunction

!------------------------------------------------------------------------------!
! Routine : init_dependency
!------------------------------------------------------------------------------!
subroutine fctset_initdependency(fctenv)
implicit none
type(st_fctfuncset) :: fctenv
integer          :: i

do i = 1, fctenv%nfct
  fctenv%fct(i)%needed = .false.
enddo

endsubroutine

!------------------------------------------------------------------------------!
! Routine : check dependency of fct function on fctenv definitions
!------------------------------------------------------------------------------!
recursive subroutine fctset_checkdependency(fctenv, fct)
implicit none
type(st_fctfuncset) :: fctenv
type(st_fct_node)   :: fct
integer             :: i

do i = 1, fctenv%nfct
  if (fctenv%fct(i)%needed) then
    ! useless to test
  else  
    fctenv%fct(i)%needed = fct_dependency(fct, fctenv%fct(i)%name)
    if (fctenv%fct(i)%needed) call fctset_checkdependency(fctenv, fctenv%fct(i)%func)
  endif
enddo

endsubroutine

endmodule FCT_FUNC
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2006 : module creation
! Mar  2013 : name of function
!------------------------------------------------------------------------------!

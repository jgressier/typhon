!------------------------------------------------------------------------------!
! Procedure : calc_sources_ext				Authors : JG
!							Date    : Feb 2011
! Function
!   Computation of external source terms for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_source_ext(umesh, field, defns, curtime)

use TYPHMAKE
use VARCOM
use OUTPUT
use USTMESH
use DEFFIELD
use EQNS
use MENU_NS
use FCT_EVAL

implicit none

! INPUTS
type(st_ustmesh)        :: umesh
type(mnu_ns)            :: defns
real(krp)               :: curtime

! INPUTS/OUTPUTS
type(st_field) :: field

! INTERNAL
integer   :: ic	! index on cells
real(krp) :: mass
type(v3d) :: dmomentum, massV
real(krp) :: dpower

! -- BODY --

call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL
call fct_env_set_real(blank_env, "t", curtime)

!! DEV : planned to add x, y, z position 

if (defns%is_extpower) then
  do ic = 1, umesh%ncell_int
    call fct_eval_real(blank_env, defns%extpower, dpower)
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + umesh%mesh%volume(ic,1,1) * dpower
  enddo
endif

if (defns%is_extforce) then
  do ic = 1, umesh%ncell_int
    call fct_eval_real(blank_env, defns%extforce_x, dmomentum%x)
    call fct_eval_real(blank_env, defns%extforce_y, dmomentum%y)
    call fct_eval_real(blank_env, defns%extforce_z, dmomentum%z)
    mass   = umesh%mesh%volume(ic,1,1) * field%etatcons%tabscal(1)%scal(ic)
    massV  = umesh%mesh%volume(ic,1,1) * field%etatcons%tabvect(1)%vect(ic)
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + (mass*dmomentum)
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + (massV.scal.dmomentum)
  enddo
endif

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

end subroutine calc_source_ext
!------------------------------------------------------------------------------!
! Changes history
!
! feb  2010 : creation, external source terms
!------------------------------------------------------------------------------!

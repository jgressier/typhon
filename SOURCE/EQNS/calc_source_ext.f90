!------------------------------------------------------------------------------!
! Procedure : calc_sources_ext				Authors : JG
!							Date    : Feb 2011
! Function
!   Computation of external source terms for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_source_ext(umesh, field, defns, curtime)

use PACKET
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
integer, pointer      :: ista(:), iend(:) ! starting and ending index
integer               :: ib, buf, nblock      ! buffer size 

! -- BODY --

if ((defns%is_extpower).or.(defns%is_extforce)) then
  call new_buf_index(umesh%ncell_int, cell_buffer, nblock, ista, iend)
  call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL
  call fct_env_set_real(blank_env, "t", curtime)
endif

!! DEV : planned to add x, y, z position 

if (defns%is_extpower) then
  !$OMP PARALLEL DO private(ic, dpower) shared(blank_env)
  do ib = 1, nblock
    buf = iend(ib)-ista(ib)+1
    do ic = ista(ib), iend(ib)
      call fct_eval_real(blank_env, defns%extpower, dpower)
      field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + umesh%mesh%volume(ic,1,1) * dpower
    enddo
  enddo
  !$OMP END PARALLEL DO
endif

if (defns%is_extforce) then
  !$OMP PARALLEL DO private(ic, mass, massV, dmomentum) shared(blank_env)
  do ib = 1, nblock
    buf = iend(ib)-ista(ib)+1
    do ic = ista(ib), iend(ib)
      call fct_eval_real(blank_env, defns%extforce_x, dmomentum%x)
      call fct_eval_real(blank_env, defns%extforce_y, dmomentum%y)
      call fct_eval_real(blank_env, defns%extforce_z, dmomentum%z)
      mass   = umesh%mesh%volume(ic,1,1) * field%etatcons%tabscal(1)%scal(ic)
      massV  = umesh%mesh%volume(ic,1,1) * field%etatcons%tabvect(1)%vect(ic)
      field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + (mass*dmomentum)
      field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + (massV.scal.dmomentum)
    enddo
  enddo
  !$OMP END PARALLEL DO
endif

if ((defns%is_extpower).or.(defns%is_extforce)) then
  deallocate(ista, iend)
  call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL
endif


end subroutine calc_source_ext
!------------------------------------------------------------------------------!
! Changes history
!
! feb  2010 : creation, external source terms
!------------------------------------------------------------------------------!

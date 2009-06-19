!------------------------------------------------------------------------------!
! Procedure : prb_grid_vol   
!
! Fonction 
!   Computation of volumic probe
!
!------------------------------------------------------------------------------!
subroutine prb_grid_vol(defsolver, grid, probe)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use FCT_EVAL
use FCT_ENV

implicit none

! -- INPUTS --
type(mnu_solver) :: defsolver
type(st_grid)    :: grid

! -- OUTPUTS --
type(mnu_probe) :: probe

! -- Internal variables --
integer(kip)      :: ic, isca, ivec
real(krp)         :: result
character(len=shortname) :: qname

! -- BODY --

call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

do ic = 1, grid%umesh%ncell_int

  call fct_env_set_real(blank_env, "x", grid%umesh%mesh%centre(ic,1,1)%x)
  call fct_env_set_real(blank_env, "y", grid%umesh%mesh%centre(ic,1,1)%y)
  call fct_env_set_real(blank_env, "z", grid%umesh%mesh%centre(ic,1,1)%z)
 
  do isca = 1, grid%info%field_loc%etatprim%nscal
    qname = quantity_name(defsolver%idsca(isca))
    call fct_env_set_real(blank_env, trim(qname), grid%info%field_loc%etatprim%tabscal(isca)%scal(ic))
  enddo
  do ivec = 1, grid%info%field_loc%etatprim%nvect
    qname = quantity_name(defsolver%idvec(ivec))
    call fct_env_set_real(blank_env, trim(qname), abs(grid%info%field_loc%etatprim%tabvect(ivec)%vect(ic)))
  enddo

  !call print_fct_env(6, blank_env)
  call fct_eval_real(blank_env, probe%quantity, result)

  select case(probe%type)
  case(vol_min)
    probe%result = min(probe%result, result)
  case(vol_max)
    probe%result = max(probe%result, result)
  case(vol_average)
    probe%result = probe%result + result*grid%umesh%mesh%volume(ic,1,1)
  case default
    call error_stop("Internal error (prb_grid_vol): unknown probe type")
  endselect
  
enddo 

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

  
! -- MPI comparison of minimum timesteps

!call exchange_zonal_timestep(lgrid, dt)


endsubroutine prb_grid_vol

!------------------------------------------------------------------------------!
! change history
!
! June 2009: created
!------------------------------------------------------------------------------!

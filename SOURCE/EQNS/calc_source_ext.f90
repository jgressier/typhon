!------------------------------------------------------------------------------!
! Procedure : calc_sources_ext				Authors : JG
!							Date    : Feb 2011
! Function
!   Computation of external source terms for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_source_ext(defsolver, umesh, field, curtime)

use PACKET
use OUTPUT
use USTMESH
use DEFFIELD
use EQNS
use MENU_SOLVER
use FCT_EVAL

implicit none

! -- INPUTS --
type(st_ustmesh)        :: umesh
type(mnu_solver)        :: defsolver
real(krp)               :: curtime

! -- INPUTS/OUTPUTS --
type(st_field) :: field

! -- INTERNAL --
integer   :: ic	! index on cells
real(krp) :: mass
type(v3d) :: dmomentum, massV
real(krp) :: dpower
integer, pointer      :: ista(:), iend(:) ! starting and ending index
integer               :: ib, buf, nblock      ! buffer size 
type(st_fct_env)      :: env
logical               :: xyz_depend

! -- BODY --

if ((defsolver%defns%is_extpower).or.(defsolver%defns%is_extforce)) then

  call new_buf_index(umesh%ncell_int, cell_buffer, nblock, ista, iend)

  call fctset_initdependency(defsolver%fctenv)
  if (defsolver%defns%is_extpower) call fctset_checkdependency(defsolver%fctenv, defsolver%defns%extpower)
  if (defsolver%defns%is_extforce) then
    call fctset_checkdependency(defsolver%fctenv, defsolver%defns%extforce_x)
    call fctset_checkdependency(defsolver%fctenv, defsolver%defns%extforce_y)
    call fctset_checkdependency(defsolver%fctenv, defsolver%defns%extforce_z)
  endif  

  if (defsolver%defns%is_extpower) then
    xyz_depend = fct_dependency(defsolver%defns%extpower, "x").or. &
                 fct_dependency(defsolver%defns%extpower, "y").or. &
                 fct_dependency(defsolver%defns%extpower, "z")
  else
    xyz_depend = .false.
  endif
  if (defsolver%defns%is_extforce) then
    xyz_depend = xyz_depend .or. &
                 fct_dependency(defsolver%defns%extforce_x, "x").or. &
                 fct_dependency(defsolver%defns%extforce_x, "y").or. &
                 fct_dependency(defsolver%defns%extforce_x, "z").or. &
                 fct_dependency(defsolver%defns%extforce_y, "x").or. &
                 fct_dependency(defsolver%defns%extforce_y, "y").or. &
                 fct_dependency(defsolver%defns%extforce_y, "z").or. &
                 fct_dependency(defsolver%defns%extforce_z, "x").or. &
                 fct_dependency(defsolver%defns%extforce_z, "y").or. &
                 fct_dependency(defsolver%defns%extforce_z, "z")
  endif

  xyz_depend = xyz_depend .or. &
               fctset_needed_dependency(defsolver%fctenv, "x").or. &
               fctset_needed_dependency(defsolver%fctenv, "y").or. &
               fctset_needed_dependency(defsolver%fctenv, "z")
               
  !$OMP PARALLEL private(ic, dpower, mass, massV, dmomentum, env) shared(ista, iend, nblock, curtime, xyz_depend)
  call new_fct_env(env)      ! temporary environment from FCT_EVAL
  call fct_env_set_real(env, "t", curtime)

  !$OMP DO
  block: do ib = 1, nblock
  
    buf = iend(ib)-ista(ib)+1
    cell: do ic = ista(ib), iend(ib)

      if (xyz_depend) then
        call fct_env_set_real(env, "X", umesh%mesh%vertex(ic,1,1)%x)
        call fct_env_set_real(env, "Y", umesh%mesh%vertex(ic,1,1)%y)
        call fct_env_set_real(env, "Z", umesh%mesh%vertex(ic,1,1)%z)
      endif

      call fctset_compute_neededenv(defsolver%fctenv, env)

      if (defsolver%defns%is_extpower) then
        call fct_eval_real(env, defsolver%defns%extpower, dpower)
        field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + umesh%mesh%volume(ic,1,1) * dpower
      endif
      if (defsolver%defns%is_extforce) then
        call fct_eval_real(env, defsolver%defns%extforce_x, dmomentum%x)
        call fct_eval_real(env, defsolver%defns%extforce_y, dmomentum%y)
        call fct_eval_real(env, defsolver%defns%extforce_z, dmomentum%z)
        mass   = umesh%mesh%volume(ic,1,1) * field%etatcons%tabscal(1)%scal(ic)
        massV  = umesh%mesh%volume(ic,1,1) * field%etatcons%tabvect(1)%vect(ic)
        field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + (mass*dmomentum)
        field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + (massV.scal.dmomentum)
      endif
      
    enddo cell
  enddo block
  
  !$OMP END DO
  call delete_fct_env(env)      ! temporary environment from FCT_EVAL
  !$OMP END PARALLEL 

  deallocate(ista, iend)

endif

end subroutine calc_source_ext
!------------------------------------------------------------------------------!
! Changes history
!
! feb  2010 : creation, external source terms
! Mar  2013 : fct environment and x,y,z dependencies
!------------------------------------------------------------------------------!

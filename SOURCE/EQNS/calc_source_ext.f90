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
integer   :: ic, il ! index on cells
real(krp), dimension(fct_buffer) :: x, y, z, dmomentx, dmomenty, dmomentz
type(v3d) :: massV
real(krp) :: mass, dpower(fct_buffer)
integer, pointer      :: ista(:), iend(:) ! starting and ending index
integer               :: ib, buf, nblock     ! buffer size 
type(st_fct_env)      :: env
logical               :: xyz_depend

! -- BODY --

if ((defsolver%defns%is_extpower).or.(defsolver%defns%is_extforce)) then

  call new_buf_index(umesh%ncell_int, fct_buffer, nblock, ista, iend)

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
               
  !$OMP PARALLEL & 
  !$OMP private(ic, il, dpower, mass, massV, dmomentx, dmomenty, dmomentz, env, x, y, z, buf) &
  !$OMP shared(ista, iend, nblock, curtime, xyz_depend) 
  
  call new_fct_env(env)      ! temporary environment from FCT_EVAL
  call fct_env_set_real(env, "t", curtime)

  !$OMP DO
  block: do ib = 1, nblock

    buf = iend(ib)-ista(ib)+1

    if (xyz_depend) then

      do ic = ista(ib), iend(ib)
        x(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%x
        y(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%y
        z(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%z
      enddo

      call fct_env_set_realarray(env, "x", x(1:buf))
      call fct_env_set_realarray(env, "y", y(1:buf))
      call fct_env_set_realarray(env, "z", z(1:buf))
    endif

    call fctset_compute_neededenv(defsolver%fctenv, env)

    if (defsolver%defns%is_extpower) then

      call fct_eval_realarray(env, defsolver%defns%extpower, dpower)
      field%residu%tabscal(2)%scal(ista(ib):iend(ib)) = field%residu%tabscal(2)%scal(ista(ib):iend(ib)) &
                                                      + umesh%mesh%volume(ista(ib):iend(ib),1,1) * dpower(1:buf)
    endif
    
    if (defsolver%defns%is_extforce) then

      call fct_eval_realarray(env, defsolver%defns%extforce_x, dmomentx)
      call fct_eval_realarray(env, defsolver%defns%extforce_y, dmomenty)
      call fct_eval_realarray(env, defsolver%defns%extforce_z, dmomentz)

      do ic = ista(ib), iend(ib)
        il = ic-ista(ib)+1
        mass  = umesh%mesh%volume(ic,1,1) * field%etatcons%tabscal(1)%scal(ic)
        massV = umesh%mesh%volume(ic,1,1) * field%etatcons%tabvect(1)%vect(ic)
        field%residu%tabvect(1)%vect(ic)%x = field%residu%tabvect(1)%vect(ic)%x + mass*dmomentx(il)
        field%residu%tabvect(1)%vect(ic)%y = field%residu%tabvect(1)%vect(ic)%y + mass*dmomenty(il)
        field%residu%tabvect(1)%vect(ic)%z = field%residu%tabvect(1)%vect(ic)%z + mass*dmomentz(il)
        field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) &
                                         + (massV%x * dmomentx(il) + massV%y * dmomenty(il) + massV%z * dmomentz(il) )
      enddo
    endif
      
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

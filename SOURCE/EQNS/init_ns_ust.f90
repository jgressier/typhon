!------------------------------------------------------------------------------!
! Procedure : init_ns_ust                 Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Initialization according parameters
!
! Defauts/Limitations/Divers :
!   CAUTION : only initialization of primitive variables
!
!------------------------------------------------------------------------------!
subroutine init_ns_ust(defsolver, init, field, umesh)

use PACKET
use DEFFIELD
use USTMESH
use EQNS
use MENU_SOLVER
use MENU_INIT
use MESHMRF
use FCT_EVAL
use FCT_ENV

implicit none

! -- INPUTS --
type(mnu_solver) :: defsolver
type(mnu_init)   :: init
type(st_ustmesh) :: umesh

! -- OUTPUTS --
type(st_field) :: field

! -- Internal variables --
integer(kip)                     :: ncell
integer                          :: ip, iloc, ic, ier, nc
character(len=50)                :: charac
real(krp)                        :: xx, yy, zz, temp
real(krp)                        :: gam, gmusd, gsgmu, rcst
integer                          :: cgnsunit
real(krp), dimension(fct_buffer) :: x, y, z, density, pstat, ptot, tstat, ttot, vel, mach
type(v3d), dimension(fct_buffer) :: velocity
integer                          :: ib, nblock, buf            ! block index and number of blocks
integer, pointer                 :: ista(:), iend(:)           ! starting and ending index
type(st_fct_env)                 :: env

! -- BODY --

!
! ncell_int should be sufficient but primitives variables are computed on ncell
ncell = umesh%ncell ! _int

!!! DEV !!! should not directly use gamma
rcst  = defsolver%defns%properties(1)%r_const  
gam   = defsolver%defns%properties(1)%gamma
gmusd = (gam-1._krp)/2._krp
gsgmu = gam/(gam-1._krp)

select case(init%type)

case(init_def)  ! --- initialization through FCT functions ---

call new_buf_index(ncell, fct_buffer, nblock, ista, iend, nthread)

!$OMP PARALLEL & 
!$OMP private(ib, buf, iloc, ic, env, x, y, z, density, pstat, ptot, tstat, ttot, vel, velocity, mach, nc) &
!$OMP shared(ista, iend, nblock, gam, gsgmu, gmusd, rcst) 
  
call new_fct_env(env)      ! temporary environment from FCT_EVAL

!$OMP DO
do ib = 1, nblock
  buf = iend(ib)-ista(ib)+1

  do iloc = 1, buf
    ic = iloc+ista(ib)-1
    x(iloc) = umesh%mesh%centre(ic,1,1)%x
    y(iloc) = umesh%mesh%centre(ic,1,1)%y
    z(iloc) = umesh%mesh%centre(ic,1,1)%z
  enddo
  call fct_env_set_realarray(env, "x", x(1:buf))
  call fct_env_set_realarray(env, "y", y(1:buf))
  call fct_env_set_realarray(env, "z", z(1:buf))

  call fctset_compute_neededenv(defsolver%fctenv, env)

  if (init%ns%is_pstat) then
    call fct_eval_realarray(env, init%ns%pstat, pstat(1:buf))
  else
    call fct_eval_realarray(env, init%ns%ptot, ptot(1:buf))
  endif

  if (init%ns%is_density) then
    call fct_eval_realarray(env, init%ns%density, density(1:buf))
  else
    if (init%ns%is_tstat) then
      call fct_eval_realarray(env, init%ns%tstat, tstat(1:buf))
    else
      call fct_eval_realarray(env, init%ns%ttot, ttot(1:buf))
    endif
  endif

  if (init%ns%is_vcomponent) then
    call fct_eval_realarray(env, init%ns%vx, x(1:buf))
    call fct_eval_realarray(env, init%ns%vy, y(1:buf))
    call fct_eval_realarray(env, init%ns%vz, z(1:buf))
    do iloc = 1, buf
      velocity(iloc)%x = x(iloc)
      velocity(iloc)%y = y(iloc)
      velocity(iloc)%z = z(iloc)
    enddo
    vel(1:buf) = abs(velocity(1:buf))
  else
    if (init%ns%is_velocity) then
      call fct_eval_realarray(env, init%ns%velocity, vel(1:buf))
    else
      call fct_eval_realarray(env, init%ns%mach, mach(1:buf))
    endif
    call fct_eval_realarray(env, init%ns%dir_x, x(1:buf))
    call fct_eval_realarray(env, init%ns%dir_y, y(1:buf))
    call fct_eval_realarray(env, init%ns%dir_z, z(1:buf))
    do iloc = 1, buf
      velocity(iloc)%x = x(iloc)
      velocity(iloc)%y = y(iloc)
      velocity(iloc)%z = z(iloc)
    enddo
  endif

  ! --- compute static pressure if needed (Mach number required) --
  if (.not.init%ns%is_pstat) then   ! ptot is defined
    if (init%ns%is_velocity) then   ! Mach number is not defined
      call error_stop("Initialization: unable to use TOTAL PRESSURE and VELOCITY combination")
    else
      pstat(1:buf)   = ptot(1:buf) / (1._krp + gmusd*mach(1:buf)**2)**gsgmu
    endif
  endif

  ! --- computation of static temperature if needed ---  
  !if (.not.init%ns%is_tstat) then ! ttot or density is defined instead
  !  if (init%ns%is_density) then
  !    tstat(1:buf) = pstat(1:buf) / (rcst * density(1:buf))
  !  else ! ttot is defined
  !    if (init%ns%is_velocity) then
  !      tstat(1:buf) = ttot(1:buf) - gmusd/gam/rcst*vel(1:buf)**2
  !    else
  !      tstat(1:buf) = ttot(1:buf) / (1._krp + gmusd*mach(1:buf)**2)
  !    endif
  !  endif
  !endif
  
  ! --- compute density if needed ---
  if (.not.init%ns%is_density) then
    if (init%ns%is_tstat) then 
      density(1:buf) = pstat(1:buf) / (rcst * tstat(1:buf))
    else ! ttot is defined
      if (init%ns%is_velocity) then
        density(1:buf) = pstat(1:buf)/(rcst*ttot(1:buf) - (gmusd/gam)*vel(1:buf)**2)
      else
        density(1:buf) = pstat(1:buf)/(rcst*ttot(1:buf)) * (1._krp + gmusd*mach(1:buf)**2)
      endif
    endif
  endif

  ! --- computation of velocity magnitude if needed ---  
  if (.not.init%ns%is_velocity) then   ! Mach number is defined
    vel(1:buf) = sqrt((gam*pstat(1:buf)/density(1:buf)))*mach(1:buf)
    !print*,'ps :',sum(pstat(1:buf))/buf
    !print*,'rho:',sum(density(1:buf))/buf
    !print*,'vel:',sum(vel(1:buf))/buf
    !print*,'m  :',sum(mach(1:buf))/buf
  endif
  ! -- velocity components if not already defined --
  if (.not.init%ns%is_vcomponent) then
    velocity(1:buf) = (vel(1:buf)/abs(velocity(1:buf)))*velocity(1:buf)
  endif

  ! -- check positivity --
  nc = count(density(1:buf) <= 0._krp)
  if (nc > 0) call error_stop("Initialization: user parameters produce " &
                          //"negative densities ("//trim(strof(nc))//" cells)" )
  nc = count(pstat(1:buf) <= 0._krp)
  if (nc > 0) call error_stop("Initialization: user parameters produce " &
                          //"negative pressures ("//trim(strof(nc))//" cells)" )
  
  ! -- compute density --
  field%etatprim%tabscal(1)%scal(ista(ib):iend(ib)) = density(1:buf)
  ! -- pressure --
  field%etatprim%tabscal(2)%scal(ista(ib):iend(ib)) = pstat(1:buf)
  ! -- velocity components if not already defined --
  field%etatprim%tabvect(1)%vect(ista(ib):iend(ib)) = velocity(1:buf)

enddo ! block loop
!$OMP END DO
call delete_fct_env(env)      ! temporary environment from FCT_EVAL
!$OMP END PARALLEL
deallocate(ista, iend)

case(init_udf)
  call print_info(5,"   UDF initialization")
  call udf_ns_init(defsolver%defns, ncell, umesh%mesh%centre(1:ncell, 1, 1), field%etatprim)

case(init_file)
  call print_info(5,"   user file initialization")
  open(unit=1004, file = init%file, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic = 1, ncell
    read(1004,'(8e18.8)') xx, yy, zz, field%etatprim%tabvect(1)%vect(ic)%x, &
                                      field%etatprim%tabvect(1)%vect(ic)%y, &
                                      field%etatprim%tabvect(1)%vect(ic)%z, &
                                      field%etatprim%tabscal(2)%scal(ic), temp
    field%etatprim%tabscal(1)%scal(ic) = field%etatprim%tabscal(2)%scal(ic) / &
                  ( temp * defsolver%defns%properties(1)%r_const )
  enddo
  close(1004)

case(init_cgns)
  call erreur("internal error", "should be called here")

case default
  call erreur("internal error", "unknown initialization method (init_ns_ust)")

endselect

! -- MRF update --

if (defsolver%defmrf%type /= mrf_none .and. defsolver%defmrf%input == mrfdata_absolute) then
  do ic = 1, ncell
    call mrfvel_abs2rel(defsolver%defmrf, 0._krp, umesh%mesh%centre(ic,1,1), field%etatprim%tabvect(1)%vect(ic))
  enddo
endif

endsubroutine init_ns_ust
!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
! Nov  2007 : add density based computation of initial state
!------------------------------------------------------------------------------!



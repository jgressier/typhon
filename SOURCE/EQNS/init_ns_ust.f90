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
subroutine init_ns_ust(defns, initns, field, umesh, init_type, initfile)

use TYPHMAKE
use DEFFIELD
use USTMESH
use EQNS
use MENU_NS
use MENU_INIT
use FCT_EVAL
use FCT_ENV

implicit none

! -- INPUTS --
type(mnu_ns)     :: defns
type(st_init_ns) :: initns
type(st_ustmesh) :: umesh
integer(kpp)     :: init_type
character(len=longname) :: initfile

! -- OUTPUTS --
type(st_field) :: field

! -- Internal variables --
integer(kip)                      :: ncell
integer                           :: ip, ifirst, iend, ic, i, ib, ier, nc
integer                           :: nb, maxbuf, buf       ! block number, buffers
type(st_nsetat)                   :: nspri
character(len=50)                 :: charac
real(krp)                         :: xx, yy, zz, temp
real(krp), dimension(cell_buffer) :: ptot, pstat, ttot, tstat, density, vel, mach
type(v3d), dimension(cell_buffer) :: velocity
logical                           :: is_x, is_y, is_z
real(krp)                         :: gamma
integer                           :: cgnsunit

! -- BODY --

ncell = umesh%ncell_int

!!! DEV !!! should not directly use gamma
gamma = defns%properties(1)%gamma

select case(init_type)

case(init_def)  ! --- initialization through FCT functions ---
  
  !is_x = 
  !
  !if (is_x.or.is_y.or.is_z) then ! test if function depends on x, y, or z

  call new(nspri, 1)
  call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

  call calc_buffer(ncell, cell_buffer, nb, maxbuf, buf)
  ifirst = 1

  do ib = 1, nb
  
    iend = ifirst+buf-1
    
    do ic = ifirst, iend

      i = ic - ifirst+1
     
      call fct_env_set_real(blank_env, "x", umesh%mesh%centre(ic,1,1)%x)
      call fct_env_set_real(blank_env, "y", umesh%mesh%centre(ic,1,1)%y)
      call fct_env_set_real(blank_env, "z", umesh%mesh%centre(ic,1,1)%z)

      if (initns%is_pstat) then
        call fct_eval_real(blank_env, initns%pstat, pstat(i))
      else
        call fct_eval_real(blank_env, initns%ptot, ptot(i))
      endif

      if (initns%is_density) then
        call fct_eval_real(blank_env, initns%density, density(i))
      else
        if (initns%is_tstat) then
          call fct_eval_real(blank_env, initns%tstat, tstat(i))
        else
          call fct_eval_real(blank_env, initns%ttot, ttot(i))
        endif
      endif

      if (initns%is_vcomponent) then
        call fct_eval_real(blank_env, initns%vx, velocity(i)%x)
        call fct_eval_real(blank_env, initns%vy, velocity(i)%y)
        call fct_eval_real(blank_env, initns%vz, velocity(i)%z)
        vel(i) = abs(velocity(i))
      else
        if (initns%is_velocity) then
          call fct_eval_real(blank_env, initns%velocity, vel(i))
        else
          call fct_eval_real(blank_env, initns%mach, mach(i))
        endif

        call fct_eval_real(blank_env, initns%dir_x, velocity(i)%x)
        call fct_eval_real(blank_env, initns%dir_y, velocity(i)%y)
        call fct_eval_real(blank_env, initns%dir_z, velocity(i)%z)
      endif
      
    enddo

    ! -- compute density & static pressure (if needed, via total/static temperature/pressure) --

    if (initns%is_pstat) then
      
      if (.not.initns%is_density) then
        call compute_tstat
        density(1:buf) = pstat(1:buf) / (defns%properties(1)%r_const * tstat(1:buf))
      endif

    else ! ptot is defined

      if (initns%is_density) then   ! must only compute pstat (without tstat/ttot)

        if (initns%is_velocity) then   ! Mach number is not defined
          call erreur("Initialization", "unable to use DENSITY, TOTAL PRESSURE and VELOCITY combination")
        else
          pstat(1:buf)   = ptot(1:buf) / (1._krp + .5_krp*(gamma-1._krp)*mach(1:buf)**2)**(gamma/(gamma-1._krp))
        endif

      else
        call compute_tstat
        if (initns%is_velocity) then   ! Mach number is not defined
          mach(1:buf) = abs(vel(1:buf))/sqrt(gamma*defns%properties(1)%r_const * tstat(1:buf))
        endif
        pstat(1:buf)   = ptot(1:buf) / (1._krp + .5_krp*(gamma-1._krp)*mach(1:buf)**2)**(gamma/(gamma-1._krp))
        density(1:buf) = pstat(1:buf) / (defns%properties(1)%r_const * tstat(1:buf))
      endif

    endif

    ! -- check positivity --
    nc = count(density(1:buf) <= 0._krp)
    if (nc > 0) call erreur("Initialization", "user parameters produce negative densities (" &
                            //trim(strof(nc))//" cells)" )
    nc = count(pstat(1:buf) <= 0._krp)
    if (nc > 0) call erreur("Initialization", "user parameters produce negative pressures (" &
                            //trim(strof(nc))//" cells)" )
    
    ! -- compute velocity (from mach number) --
    if (.not.initns%is_velocity) then
      vel(1:buf) = sqrt(gamma*pstat(1:buf)/density(1:buf))*mach(1:buf)
    endif
    
    ! -- compute density --
    field%etatprim%tabscal(1)%scal(ifirst:iend) = density(1:buf)

    ! -- pressure --
    field%etatprim%tabscal(2)%scal(ifirst:iend) = pstat(1:buf)

    ! -- velocity components if not already defined --
    if (initns%is_vcomponent) then
      field%etatprim%tabvect(1)%vect(ifirst:iend) = velocity(1:buf)
    else
      field%etatprim%tabvect(1)%vect(ifirst:iend) = (vel(1:buf)/abs(velocity(1:buf)))*velocity(1:buf)
    endif

    ifirst = ifirst + buf
    buf    = maxbuf
  enddo
  
  call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

  call delete(nspri)
  !!if (field%allocgrad) field%gradient(:,:,:,:,:) = 0._krp

case(init_udf)
  call print_info(5,"   UDF initialization")
  call udf_ns_init(defns, ncell, umesh%mesh%centre(1:ncell, 1, 1), field%etatprim)

case(init_file)
  call print_info(5,"   user file initialization")
  open(unit=1004, file = initfile, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,'(8e18.8)') xx, yy, zz, field%etatprim%tabvect(1)%vect(ic)%x, &
                                      field%etatprim%tabvect(1)%vect(ic)%y, &
                                      field%etatprim%tabvect(1)%vect(ic)%z, &
                                      field%etatprim%tabscal(2)%scal(ic), temp
    field%etatprim%tabscal(1)%scal(ic) = field%etatprim%tabscal(2)%scal(ic) / &
                  ( temp * defns%properties(1)%r_const )
  enddo
  close(1004)

case(init_cgns)
  call erreur("internal error", "should be called here")

case default
  call erreur("internal error", "unknown initialization method (init_ns_ust)")

endselect

! -------- SUBROUTINES ------------
contains

subroutine compute_tstat ! macro-like, using "routine global" variables
implicit none
  if (.not.initns%is_tstat) then ! ttot is defined
    if (initns%is_velocity) then
      tstat(1:buf) = ttot(1:buf) - .5_krp*(gamma-1._krp)/gamma/defns%properties(1)%r_const*vel(1:buf)**2
    else
      tstat(1:buf) = ttot(1:buf) / (1._krp + .5_krp*(gamma-1._krp)*mach(1:buf)**2)
    endif
  endif
endsubroutine compute_tstat

endsubroutine init_ns_ust

!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
! Nov  2007 : add density based computation of initial state
!------------------------------------------------------------------------------!



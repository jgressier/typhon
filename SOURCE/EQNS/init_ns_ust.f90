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
subroutine init_ns_ust(defns, initns, champ, mesh, init_type, initfile, ncell)

use TYPHMAKE
use DEFFIELD
use EQNS
use MENU_NS
use MENU_INIT
use FCT_EVAL
use FCT_ENV

implicit none

! -- INPUTS --
type(mnu_ns)     :: defns
type(st_init_ns) :: initns
type(st_mesh)    :: mesh
integer(kpp)     :: init_type
character(len=strlen) :: initfile
integer(kip)     :: ncell

! -- OUTPUTS --
type(st_field) :: champ

! -- Internal variables --
integer                           :: ip, ifirst, iend, ic, i, ib, ierr, nc
integer                           :: nb, maxbuf, buf       ! block number, buffers
type(st_nsetat)                   :: nspri
character(len=50)                 :: charac
real(krp)                         :: xx, yy, zz, temp
real(krp), dimension(cell_buffer) :: ptot, pstat, ttot, tstat, vel, mach
type(v3d), dimension(cell_buffer) :: velocity
logical                           :: is_x, is_y, is_z
real(krp)                         :: gamma

! -- BODY --

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
      
      
      call fct_env_set_real(blank_env, "x", mesh%centre(ic,1,1)%x)
      call fct_env_set_real(blank_env, "y", mesh%centre(ic,1,1)%y)
      call fct_env_set_real(blank_env, "z", mesh%centre(ic,1,1)%z)
      if (initns%is_pstat) then
        call fct_eval_real(blank_env, initns%pstat, pstat(i))
      else
        call fct_eval_real(blank_env, initns%ptot, ptot(i))
      endif
      if (initns%is_tstat) then
        call fct_eval_real(blank_env, initns%tstat, tstat(i))
      else
        call fct_eval_real(blank_env, initns%ttot, ttot(i))
      endif
      if (initns%is_velocity) then
        call fct_eval_real(blank_env, initns%velocity, vel(i))
      else
        call fct_eval_real(blank_env, initns%mach, mach(i))
      endif
      call fct_eval_real(blank_env, initns%dir_x, velocity(i)%x)
      call fct_eval_real(blank_env, initns%dir_y, velocity(i)%y)
      call fct_eval_real(blank_env, initns%dir_z, velocity(i)%z)
    enddo

    ! -- compute static temperature --
    if (.not.initns%is_tstat) then
      if (initns%is_velocity) then
        tstat(1:buf) = ttot(1:buf) - .5_krp*(gamma-1._krp)/gamma/defns%properties(1)%r_const*vel(1:buf)**2
      else
        tstat(1:buf) = ttot(1:buf) / (1._krp + .5_krp*(gamma-1._krp)*mach(1:buf)**2)
      endif 
    endif
    ! -- check positivity --
    nc = count(tstat(1:buf) <= 0._krp)
    if (nc > 0) call erreur("Initialization", "user parameters produce negative temperatures (" &
                            //trim(strof(nc))//" cells)" )
    
    ! -- compute velocity (from mach number) --
    if (.not.initns%is_velocity) then
      vel(1:buf) = sqrt(gamma*defns%properties(1)%r_const*tstat(1:buf))*mach(1:buf)
    endif
    
    ! -- compute static pressure (from pi & mach number) --
    if (.not.initns%is_pstat) then
      if (initns%is_velocity) then   ! Mach number is not defined
        mach(1:buf) = abs(vel(1:buf))/sqrt(gamma*defns%properties(1)%r_const*tstat(1:buf))
      endif
      pstat(1:buf) = ptot(1:buf) / (1._krp + .5_krp*(gamma-1._krp)*mach(1:buf)**2)**(gamma/(gamma-1._krp))
    endif
    
    ! -- compute density --
    champ%etatprim%tabscal(1)%scal(ifirst:iend) = pstat(1:buf)/(defns%properties(1)%r_const*tstat(1:buf))

    ! -- pressure --
    champ%etatprim%tabscal(2)%scal(ifirst:iend) = pstat(1:buf)

    ! -- velocity vector --
    champ%etatprim%tabvect(1)%vect(ifirst:iend) = (vel(1:buf)/abs(velocity(1:buf)))*velocity(1:buf)

    ifirst = ifirst + buf
    buf    = maxbuf
  enddo
  
  call delete(nspri)
  !!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

case(init_udf)
  print*,"   UDF initialization"
  call udf_ns_init(defns, ncell, mesh%centre(1:ncell, 1, 1), champ%etatprim)

case(init_file)
  print*,"   File initialization"
  open(unit=1004, file = initfile, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,'(8e18.8)') xx, yy, zz, champ%etatprim%tabvect(1)%vect(ic)%x, &
                                      champ%etatprim%tabvect(1)%vect(ic)%y, &
                                      champ%etatprim%tabvect(1)%vect(ic)%z, &
                                      champ%etatprim%tabscal(2)%scal(ic), temp
    champ%etatprim%tabscal(1)%scal(ic) = champ%etatprim%tabscal(2)%scal(ic) / &
                  ( temp * defns%properties(1)%r_const )
  enddo
  close(1004)

case default
  call erreur("internal error", "unknown initialization method (init_ns_ust)")

endselect

endsubroutine init_ns_ust

!------------------------------------------------------------------------------!
! Modification history
!
! july 2004 : creation & calculation of uniform primitive variables
!------------------------------------------------------------------------------!



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
use MENU_NS
use MENU_INIT
use FCT_EVAL
use FCT_ENV

implicit none

! -- Declaration des entrees --
type(mnu_ns)     :: defns
type(st_init_ns) :: initns
type(st_mesh)    :: mesh
integer(kpp)     :: init_type
character(len=strlen) :: initfile
integer(kip)     :: ncell

! -- Declaration des sorties --
type(st_field) :: champ

! -- Declaration des variables internes --
integer         :: ip, ic, ierr
type(st_nsetat) :: nspri
character(len=50):: charac
real(krp)       :: x, y, z, temp, ptot, ttot, mach
logical         :: is_x, is_y, is_z

! -- BODY --


select case(init_type)

case(init_def)  ! --- initialization through FCT functions ---
  
  !is_x = 
  !
  !if (is_x.or.is_y.or.is_z) then ! test if function depends on x, y, or z

  call new(nspri, 1)
  call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

  do ic = 1, ncell
    call fct_env_set_real(blank_env, "x", mesh%centre(ic,1,1)%x)
    call fct_env_set_real(blank_env, "y", mesh%centre(ic,1,1)%y)
    call fct_env_set_real(blank_env, "z", mesh%centre(ic,1,1)%z)
    call fct_eval_real(blank_env, initns%ptot, ptot)
    call fct_eval_real(blank_env, initns%ttot, ttot)
    call fct_eval_real(blank_env, initns%mach, mach)
    call pi_ti_mach_dir2nspri(defns%properties(1), 1, ptot, ttot, mach, initns%direction, nspri) 
    champ%etatprim%tabscal(1)%scal(ic) = nspri%density(1)
    champ%etatprim%tabscal(2)%scal(ic) = nspri%pressure(1)
    champ%etatprim%tabvect(1)%vect(ic) = nspri%velocity(1)
  enddo

  call delete(nspri)
  !!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

case(init_udf)
  print*,"   UDF initialization"
  call udf_ns_init(defns, ncell, mesh%centre(1:ncell, 1, 1), champ%etatprim)

case(init_file)
  print*, initfile
  open(unit=1004, file = initfile, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,'(8e18.8)') x, y, z, champ%etatprim%tabvect(1)%vect(ic)%x, &
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



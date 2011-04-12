!------------------------------------------------------------------------------!
! Procedure : init_kdif_ust               Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : juin 2003 (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!   ATTENTION : initialisation des variables primitives
!
!------------------------------------------------------------------------------!
subroutine init_kdif_ust(init, field, umesh)

use TYPHMAKE
use DEFFIELD
use USTMESH
use MENU_KDIF
use MENU_INIT

implicit none

! -- INPUTS --
type(mnu_init)   :: init
type(st_ustmesh) :: umesh

! -- OUTPUTS --
type(st_field) :: field

! -- Internal Variables --
integer(kip)          :: ncell
integer               :: ip, ic
character(len=50)     :: charac
real(krp)             :: x,y,z

! -- BODY --

ncell = umesh%ncell_int 

select case(init%type)

case(init_def)

  if (init%unif == init_unif) then
    do ip = 1, field%nscal
      field%etatprim%tabscal(ip)%scal(:) = init%kdif%temp
    enddo
  else !provisoire
    do ip = 1, field%nscal
      do ic=1, field%ncell
       field%etatprim%tabscal(ip)%scal(ic)=init%kdif%coef(1)*umesh%mesh%centre(ic,1,1)%x+&
                                           init%kdif%coef(2)*umesh%mesh%centre(ic,1,1)%y+&
                                           init%kdif%coef(3)*umesh%mesh%centre(ic,1,1)%z+&
                                           init%kdif%coef(4)
      enddo
    enddo
  endif

! pas de de variables vectorielles attendues (pas de test)

!!if (field%allocgrad) field%gradient(:,:,:,:,:) = 0._krp

case(init_file)
  open(unit=1004, file = init%file, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,*) x, y, z, field%etatprim%tabscal(1)%scal(ic)
  enddo
  close(1004)

endselect

endsubroutine init_kdif_ust

!------------------------------------------------------------------------------!
! Changes History
!
! mars 2003 (v0.0.1b) : creation de la routine
! juin 2003           : maj pour variables conservatives et primitives
!------------------------------------------------------------------------------!



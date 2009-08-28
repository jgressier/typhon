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
subroutine init_kdif_ust(kdif, champ, unif, umesh, init_type, initfile)

use TYPHMAKE
use DEFFIELD
use USTMESH
use MENU_KDIF
use MENU_INIT

implicit none

! -- INPUTS --
type(st_init_kdif)    :: kdif
integer               :: unif ! uniformite de la condition initiale
type(st_ustmesh)      :: umesh
integer(kip)     :: init_type
character(len=longname) :: initfile

! -- OUTPUTS --
type(st_field) :: champ

! -- Internal Variables --
integer(kip)          :: ncell
integer               :: ip, ic
character(len=50)     :: charac
real(krp)             :: x,y,z

! -- BODY --

ncell = umesh%ncell_int 

select case(init_type)

case(init_def)

  if (unif == init_unif) then
    do ip = 1, champ%nscal
      champ%etatprim%tabscal(ip)%scal(:) = kdif%temp
    enddo
  else !provisoire
    do ip = 1, champ%nscal
      do ic=1, champ%ncell
       champ%etatprim%tabscal(ip)%scal(ic)=kdif%coef(1)*umesh%mesh%centre(ic,1,1)%x+&
                                           kdif%coef(2)*umesh%mesh%centre(ic,1,1)%y+&
                                           kdif%coef(3)*umesh%mesh%centre(ic,1,1)%z+&
                                           kdif%coef(4)
      enddo
    enddo
  endif

! pas de de variables vectorielles attendues (pas de test)

!!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp

case(init_file)
  open(unit=1004, file = initfile, form="formatted")
  read(1004,'(a)') charac
  read(1004,'(a)') charac
  do ic=1, ncell
    read(1004,*) x, y, z, champ%etatprim%tabscal(1)%scal(ic)
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



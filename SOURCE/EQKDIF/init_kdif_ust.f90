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
subroutine init_kdif_ust(kdif, champ, unif, mesh, profil)

use TYPHMAKE
use DEFFIELD
use MENU_KDIF
use MENU_INIT

implicit none

! -- Declaration des entrees --
type(st_init_kdif) :: kdif
integer            :: unif ! uniformite de la condition initiale
type(st_mesh)      :: mesh
integer            :: profil ! profil de temperature initial

! -- Declaration des sorties --
type(st_field) :: champ

! -- Declaration des variables internes --
integer :: ip, ic

! -- Debut de la procedure --

if (unif == init_unif) then
  do ip = 1, champ%nscal
    champ%etatprim%tabscal(ip)%scal(:) = kdif%temp
  enddo
else !provisoire
  select case(profil)
  case(lin)
    do ip = 1, champ%nscal
      do ic=1, champ%ncell
       champ%etatprim%tabscal(ip)%scal(ic)=kdif%coef(1)*mesh%centre(ic,1,1)%x+&
                                          kdif%coef(2)*mesh%centre(ic,1,1)%y+&
                                          kdif%coef(3)*mesh%centre(ic,1,1)%z+&
                                          kdif%coef(4)
      enddo
    enddo

  case(step)
    do ip = 1, champ%nscal
      do ic=1, champ%ncell
        if ( (mesh%centre(ic,1,1)%x .lt. kdif%coef(3)).and. &
             (mesh%centre(ic,1,1)%y .lt. kdif%coef(4)).and. &
             (mesh%centre(ic,1,1)%z .lt. kdif%coef(5)) ) then
          champ%etatprim%tabscal(ip)%scal(ic)=kdif%coef(1)
        else
          champ%etatprim%tabscal(ip)%scal(ic)=kdif%coef(2)
        endif
      enddo
    enddo

  case default
    call erreur("incoherence interne (init_kdif_ust)","profil initial inconnu")
  endselect

endif

! pas de de variables vectorielles attendues (pas de test)

!!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp


endsubroutine init_kdif_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b) : creation de la routine
! juin 2003           : maj pour variables conservatives et primitives
!------------------------------------------------------------------------------!



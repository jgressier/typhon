!------------------------------------------------------------------------------!
! Procedure : init_vort_ust               Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Initialisation des champs de VORTEX
!
! Defauts/Limitations/Divers :
!   ATTENTION : initialisation des variables primitives
!
!------------------------------------------------------------------------------!
subroutine init_vort_ust(vort, champ)

use TYPHMAKE
use DEFFIELD
use MENU_VORTEX

implicit none

! -- Declaration des entrées --
type(st_init_vort) :: vort

! -- Declaration des sorties --
type(st_field) :: champ

! -- Declaration des variables internes --
integer :: ip
! -- Debut de la procedure --

!do ip = 1, champ%nscal
!  champ%etatprim%tabscal(ip)%scal(:) = vort%temp
!enddo

! pas de de variables vectorielles attendues (pas de test)

!!if (champ%allocgrad) champ%gradient(:,:,:,:,:) = 0._krp


endsubroutine init_vort_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : création de la routine
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : init_capteurs               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des capteurs
!     - capteurs par défaut
!     - capteurs défnis par l'utilisateur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_capteurs(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE

implicit none

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone

! -- Declaration des variables internes --
integer :: i             ! index de domaine/capteurs

! -- Debut de la procedure --

do i = 1, zone%defsolver%nprobe

  select case(zone%info%typ_temps)

  case(stationnaire)

  case(instationnaire)
  
  case(periodique)
  
  endselect

enddo

endsubroutine init_capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! jan  2004 : création de la procédure
! july 2004 : initialization of default probes
!------------------------------------------------------------------------------!

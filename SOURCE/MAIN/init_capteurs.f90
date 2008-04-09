!------------------------------------------------------------------------------!
! Procedure : init_capteurs               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des capteurs
!     - capteurs par defaut
!     - capteurs defnis par l'utilisateur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_capteurs(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MENU_GEN

implicit none

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des variables internes --
integer :: i             ! index de domaine/capteurs

! -- Debut de la procedure --

select case(zone%info%time_model)

case(time_steady)
  !!! DEV : GESTION des entrees/sorties et numeros d'unites par MODULE
  open(unit=uf_monres, file="monres."//strof_full_int(1,3), form = "formatted")
  write(uf_monres,'(a)') "@variables: it residual"

case(time_unsteady)
  !!! DEV : GESTION des entrees/sorties et numeros d'unites par MODULE
  open(unit=uf_monphy, file="monphy."//strof_full_int(1,3), form = "formatted")
  write(uf_monphy,'(a)') "@variables: time"
  
case default
   call erreur("internal error (init_capteurs)", "unknown time model")
 
endselect

do i = 1, zone%defsolver%nprobe

  select case(zone%info%time_model)

  case(time_steady)
    call erreur("developpement","mode non traite l'initialisation des capteurs")

  case(time_unsteady)
    call erreur("developpement","mode non traite l'initialisation des capteurs")
  
  case default
   call erreur("internal error (init_capteurs)", "unknown time model")
  endselect

enddo

endsubroutine init_capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! jan  2004 : creation de la procedure
! july 2004 : initialization of default probes
!------------------------------------------------------------------------------!

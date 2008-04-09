!------------------------------------------------------------------------------!
! Procedure : init_zone               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des zone
!     - zone par defaut
!     - zone defnis par l'utilisateur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_zone(zone, prj)

use TYPHMAKE
use DEFZONE
use MENU_GEN

implicit none

! -- Declaration des entrees --
type(mnu_project) :: prj

! -- Declaration des sorties --

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des variables internes --

! -- Debut de la procedure --

zone%info%time_model =  prj%time_model

endsubroutine init_zone

!------------------------------------------------------------------------------!
! Changes history
!
! aug  2004 : creation
!------------------------------------------------------------------------------!

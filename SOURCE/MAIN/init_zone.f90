!------------------------------------------------------------------------------!
! Procedure : init_zone               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des zone
!     - zone par defaut
!     - zone defnis par l'utilisateur
!
!------------------------------------------------------------------------------!
subroutine init_zone(zone, prj)

use TYPHMAKE
use DEFZONE
use MENU_GEN

implicit none

! -- Inputs --
type(mnu_project) :: prj

! -- Inputs/Outputs --
type(st_zone) :: zone

! -- BODY --

zone%info%time_model =  prj%time_model

endsubroutine init_zone
!------------------------------------------------------------------------------!
! Changes history
!
! aug  2004 : creation
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : def_boco_ns                 Auteur : J. Gressier
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_boco_ns(block, type)!, boco)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_NS

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block    ! bloc RPM contenant les définitions
integer                :: type     ! type de condition aux limites

! -- Declaration des sorties --
!type(st_boco_ns) :: boco

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

pblock => block

select case(type)

case(bc_wall_adiab)
  call erreur("Développement","'bc_wall_adiab' : Cas non implémenté")

case(bc_wall_isoth)
  !call rpmgetkeyvalreal(pblock, "WALL_TEMP", boco%temp_wall)

case(bc_wall_flux)
  call erreur("Développement","'bc_wall_isoth' : Cas non implémenté")

case(bc_wall_hconv)
  call erreur("Développement","'bc_wall_hconv' : Cas non implémenté")

case default
  call erreur("Lecture de menu","type de conditions aux limites non reconnu&
              & pour le solveur de conduction")
endselect


endsubroutine def_boco_ns


!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov 2003 : création de la routine
!------------------------------------------------------------------------------!



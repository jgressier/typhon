!------------------------------------------------------------------------------!
! Procedure : def_spat                  Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_spat(block, defspat)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_spat) :: defspat

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition des paramètres de discrétisation spatiale")

! -- Recherche du BLOCK:SPAT_PARAM

pblock => block
call seekrpmblock(pblock, "SPAT_PARAM", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc SPAT_PARAM inexistant ou surnuméraire")



endsubroutine def_spat

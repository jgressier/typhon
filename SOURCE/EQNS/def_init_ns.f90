!------------------------------------------------------------------------------!
! Procedure : def_init_ns                 Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : 
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init_ns(block)!, init)

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
!type(st_init_ns) :: init

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

pblock => block

!call rpmgetkeyvalreal(pblock, "TEMP", init%temp)


endsubroutine def_init_ns


!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2003 : création de la routine
!------------------------------------------------------------------------------!



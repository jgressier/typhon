!------------------------------------------------------------------------------!
! Procedure : def_init_vortex             Auteur : J. Gressier
!                                         Date   : Février 2004
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres d'initialisation (domaine de solveur VORTEX)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init_vortex(block, init)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_VORTEX

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block    ! bloc RPM contenant les définitions
integer                :: type     ! type de condition aux limites

! -- Declaration des sorties --
type(st_init_vort)     :: init

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

pblock => block

!call rpmgetkeyvalreal(pblock, "TEMP", init%temp)


endsubroutine def_init_vortex


!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : création de la routine
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : def_init_vortex             Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres d'initialisation (domaine de solveur VORTEX)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init_vortex(block, initv)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_VORTEX

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites

! -- Declaration des sorties --
type(st_init_vort)     :: initv

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

pblock => block

!call rpmgetkeyvalreal(pblock, "TEMP", init%temp)


endsubroutine def_init_vortex


!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation de la routine
!------------------------------------------------------------------------------!



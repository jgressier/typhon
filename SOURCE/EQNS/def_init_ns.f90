!------------------------------------------------------------------------------!
! Procedure : def_init_ns                 Auteur : J. Gressier
!                                         Date   : Juillet 2004
! Fonction                                Modif  : cf historique
!   Traitement des parametres du fichier menu principal
!   Parametres d'initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init_ns(block, initns)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_NS

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites

! -- Declaration des sorties --
type(st_init_ns) :: initns

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

pblock => block

call rpmgetkeyvalreal(pblock, "PI",        initns%ptot)
call rpmgetkeyvalreal(pblock, "TI",        initns%ttot)
call rpmgetkeyvalreal(pblock, "MACH",      initns%mach)
call rpmgetkeyvalstr (pblock, "DIRECTION", str)
initns%direction = v3d_of(str, info)
if (info /= 0) &
  call erreur("lecture de menu","probleme a la lecture du vecteur DIRECTION") 
initns%direction = initns%direction / abs(initns%direction)

endsubroutine def_init_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2004 : creation de la routine
!------------------------------------------------------------------------------!



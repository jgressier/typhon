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
use FCT_PARSER

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

! -- BODY --

pblock => block

call rpmgetkeyvalstr(pblock, "PI", str)
print*," parsing "//trim(str)
call string_to_funct(str, initns%ptot, info)

call rpmgetkeyvalstr(pblock, "TI", str)
print*," parsing "//trim(str)
call string_to_funct(str, initns%ttot, info)

call rpmgetkeyvalstr(pblock, "MACH", str)
print*," parsing "//trim(str)
call string_to_funct(str, initns%mach, info)

call rpmgetkeyvalstr (pblock, "DIRECTION", str)
initns%direction = v3d_of(str, info)
if (info /= 0) &
  call erreur("menu definition","problem when parsing DIRECTION vector (NS initialization)") 

initns%direction = initns%direction / abs(initns%direction)

endsubroutine def_init_ns

!------------------------------------------------------------------------------!
! Changes history
!
! juil 2004 : creation de la routine
! sept 2006 : FCT functions for Ptot, Ttot ans Mach
!------------------------------------------------------------------------------!



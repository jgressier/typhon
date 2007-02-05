!------------------------------------------------------------------------------!
! Procedure : def_initns                 Auteur : J. Gressier
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

! -- INPUTS --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites

! -- OUTPUTS --
type(st_init_ns) :: initns

! -- internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

pblock => block

if (rpm_existkey(pblock, "PI")) then
  initns%is_pstat = .false.
  call rpmgetkeyvalstr(pblock, "PI", str)
  print*," parsing PI = "//trim(str)
  call string_to_funct(str, initns%ptot, info)
else
  initns%is_pstat = .true.
  call rpmgetkeyvalstr(pblock, "P", str)
  print*," parsing P = "//trim(str)
  call string_to_funct(str, initns%pstat, info)
endif

if (rpm_existkey(pblock, "TI")) then
  initns%is_tstat = .false.
  call rpmgetkeyvalstr(pblock, "TI", str)
  print*," parsing TI = "//trim(str)
  call string_to_funct(str, initns%ttot, info)
else
  initns%is_tstat = .true.
  call rpmgetkeyvalstr(pblock, "T", str)
  print*," parsing T = "//trim(str)
  call string_to_funct(str, initns%tstat, info)
endif

if (rpm_existkey(pblock, "MACH")) then
  initns%is_velocity = .false.
  call rpmgetkeyvalstr(pblock, "MACH", str)
  print*," parsing MACH = "//trim(str)
  call string_to_funct(str, initns%mach, info)
else
  initns%is_velocity = .true.
  call rpmgetkeyvalstr(pblock, "VELOCITY", str)
  print*," parsing VELOCITY = "//trim(str)
  call string_to_funct(str, initns%velocity, info)
endif

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

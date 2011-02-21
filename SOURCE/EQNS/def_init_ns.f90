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

! -- INPUTS --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites

! -- OUTPUTS --
type(st_init_ns) :: initns

! -- internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
type(v3d)                :: direction

! -- BODY --

pblock => block

if (rpm_existkey(pblock, "PI")) then
  initns%is_pstat = .false.
  call rpmgetkeyvalstr(pblock, "PI", str)
  call string_to_funct(str, initns%ptot, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
else
  initns%is_pstat = .true.
  call rpmgetkeyvalstr(pblock, "P", str)
  call string_to_funct(str, initns%pstat, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
endif

if (rpm_existkey(pblock, "DENSITY")) then
  initns%is_density = .true.
  call rpmgetkeyvalstr(pblock, "DENSITY", str)
  call string_to_funct(str, initns%density, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  if (rpm_existkey(pblock, "TI")) call erreur("NS initialization", "over-defined state (TI)")
  if (rpm_existkey(pblock, "T"))  call erreur("NS initialization", "over-defined state (T)")
  initns%is_tstat = .false.
elseif (rpm_existkey(pblock, "TI")) then
  initns%is_density = .false.
  initns%is_tstat   = .false.
  call rpmgetkeyvalstr(pblock, "TI", str)
  call string_to_funct(str, initns%ttot, info)
if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
else
  initns%is_density = .false.
  initns%is_tstat   = .true.
  call rpmgetkeyvalstr(pblock, "T", str)
  call string_to_funct(str, initns%tstat, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
endif

if (rpm_existkey(pblock, "VX")) then
  initns%is_vcomponent = .true.
  initns%is_velocity   = .true.  ! same kind of definition
  call rpmgetkeyvalstr(pblock, "VX", str)
  call convert_to_funct(str, initns%vx, info)  
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pblock, "VY", str)
  call convert_to_funct(str, initns%vy, info)  
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pblock, "VZ", str)
  call convert_to_funct(str, initns%vz, info)
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
else
  initns%is_vcomponent = .false.
  if (rpm_existkey(pblock, "MACH")) then
    initns%is_velocity = .false.
    call rpmgetkeyvalstr(pblock, "MACH", str)
    call string_to_funct(str, initns%mach, info)
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  else
    initns%is_velocity = .true.
    call rpmgetkeyvalstr(pblock, "VELOCITY", str)
    call string_to_funct(str, initns%velocity, info)
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  endif

  if (rpm_existkey(pblock, "DIRECTION")) then
    call rpmgetkeyvalstr (pblock, "DIRECTION", str)
    direction = v3d_of(str, info)
    if (info /= 0) &
         call error_stop("menu definition: problem when parsing DIRECTION vector (NS initialization)") 
    call convert_to_funct(direction%x, initns%dir_x, info)
    call convert_to_funct(direction%y, initns%dir_y, info)
    call convert_to_funct(direction%z, initns%dir_z, info)
  else
    call rpmgetkeyvalstr(pblock, "DIR_X", str)
    call convert_to_funct(str, initns%dir_x, info)  
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
    call rpmgetkeyvalstr(pblock, "DIR_Y", str)
    call convert_to_funct(str, initns%dir_y, info)  
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
    call rpmgetkeyvalstr(pblock, "DIR_Z", str)
    call convert_to_funct(str, initns%dir_z, info)  
    if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  endif
endif

endsubroutine def_init_ns

!------------------------------------------------------------------------------!
! Changes history
!
! juil 2004 : creation de la routine
! sept 2006 : FCT functions for Ptot, Ttot ans Mach
! Nov  2007 : FCT function for direction
!------------------------------------------------------------------------------!

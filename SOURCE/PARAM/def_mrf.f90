!------------------------------------------------------------------------------!
! Procedure : def_mrf                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_mrf(block, defmrf)

use RPM
use TYPHMAKE
use OUTPUT
use MATH
use MESHMRF

implicit none

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_mrf)  :: defmrf

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: info
real(krp)                :: omega_rpm
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

! looking for BLOCK:MRF --

pblock => block
call seekrpmblock(pblock, "MRF", 0, pcour, nkey)

select case(nkey)

case(0) 
  defmrf%type = mrf_none

case(1)
  call print_info(5,"- Definition of Moving Reference Frame (MRF)")

  call rpmgetkeyvalstr(pcour, "TYPE", str, "ROTATION")

  defmrf%type = -1
  if (samestring(str,"NONE"))                    defmrf%type = mrf_none
  if (samestring(str,"TRANSLATION"))             defmrf%type = mrf_trans_cst
  if (samestring(str,"OSCILLATING_TRANSLATION")) defmrf%type = mrf_trans_osc
  if (samestring(str,"ACCELERATION"))            defmrf%type = mrf_trans_lin
  if (samestring(str,"ROTATION"))                defmrf%type = mrf_rot_cst
  if (samestring(str,"OSCILLATING_ROTATION"))    defmrf%type = mrf_rot_osc

  defmrf%axis = v3d(0._krp, 0._krp, 1._krp)     

  select case(defmrf%type)
  case(mrf_none)
    call print_info(7,"  no moving reference frame defined")
  case(mrf_trans_cst)
    call print_info(7,"  constant translational velocity")
    call rpmgetkeyvalstr(pcour, "CENTER_VELOCITY", str)
    defmrf%velocity = v3d_of(str, info)     
  case(mrf_trans_lin)
    call print_info(7,"  constant (translational) acceleration")
    call rpmgetkeyvalstr(pcour, "CENTER_ACCELERATION", str)
    defmrf%acceleration = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "CENTER_VELOCITY", str, "(0., 0., 0.)")
    defmrf%velocity = v3d_of(str, info)     
  case(mrf_trans_osc)
    call print_info(7,"  oscillating translational velocity")
    call error_stop("parameters parsing: oscillating translational velocity not yet implemented")
  case(mrf_rot_cst)
    call print_info(7,"  constant rotational velocity")
    call rpmgetkeyvalstr(pcour, "CENTER", str, "(0., 0., 0.)")
    defmrf%center = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "AXIS", str, "(0., 0., 1.)")
    defmrf%axis = v3d_of(str, info)     
    if (rpm_existkey(pcour, "ROTATION_RPM")) then
      call rpmgetkeyvalreal(pcour, "OMEGA_RPM",  omega_rpm)
      defmrf%omega = 2._krp*pi*omega_rpm/60._krp
    else
      call rpmgetkeyvalreal(pcour, "OMEGA",  defmrf%omega)
    endif
    
  case(mrf_rot_lin)
    call print_info(7,"  accelerating rotation")
    call error_stop("parameters parsing: accelerating rotation not yet implemented")
  case(mrf_rot_osc)
    call print_info(7,"  oscillating rotation")
    call rpmgetkeyvalstr(pcour, "CENTER", str, "(0., 0., 0.)")
    defmrf%center = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "AXIS", str, "(0., 0., 1.)")
    defmrf%axis = v3d_of(str, info)     
    if (rpm_existkey(pcour, "ROTATION_RPM")) then
      call rpmgetkeyvalreal(pcour, "OMEGA_RPM",  omega_rpm)
      defmrf%omega = 2._krp*pi*omega_rpm/60._krp
    else
      call rpmgetkeyvalreal(pcour, "OMEGA",  defmrf%omega, 0._krp)
    endif
    call rpmgetkeyvalreal(pcour, "OSC_ANGLE",  defmrf%osc_angle)
    defmrf%osc_angle = pi/180._krp*defmrf%osc_angle
    call rpmgetkeyvalreal(pcour, "OSC_PERIOD", defmrf%osc_period)
  case default
    call error_stop("parameters parsing: unknown MRF type definition")
  endselect

  if (abs(defmrf%axis) < epsilon(1._krp)) then
    call error_stop("parameters parsing: invalid rotation axis (null module) for the Moving Reference Frame")
  else
    defmrf%axis = defmrf%axis/abs(defmrf%axis)
  endif

case default
  call error_stop("parameters parsing: too many MRF blocks found")
endselect

endsubroutine def_mrf
!------------------------------------------------------------------------------!
! Changes history
!
! dec  2010 : creation, MRF parameters parsing (A. Gardi & JG)
!------------------------------------------------------------------------------!

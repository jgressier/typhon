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

  call rpmgetkeyvalstr(pcour, "TYPE", str, "NONE")
  defmrf%type = -1
  if (samestring(str,"NONE"))                    defmrf%type = mrf_none
  if (samestring(str,"TRANSLATION"))             defmrf%type = mrf_trans_lin
  if (samestring(str,"OSCILLATING_TRANSLATION")) defmrf%type = mrf_trans_osc
  if (samestring(str,"ROTATION"))                defmrf%type = mrf_rot_cst
  if (samestring(str,"OSCILLATING_ROTATION"))    defmrf%type = mrf_rot_osc
  if (samestring(str,"COMBINED_OSCILLATION"))    defmrf%type = mrf_comb_osc

  call rpmgetkeyvalstr(pcour, "INPUT", str)
  defmrf%input = -1
  if (samestring(str,"ABSOLUTE")) then
    defmrf%input = mrfdata_absolute
  elseif (samestring(str,"RELATIVE")) then
    defmrf%input = mrfdata_relative
  else
    call error_stop("parameters parsing: unknown or missing INPUT type for the moving reference frame")
  endif

  call rpmgetkeyvalstr(pcour, "OUTPUT", str)
  defmrf%output = -1
  if (samestring(str,"ABSOLUTE")) then
    defmrf%output = mrfdata_absolute
    call error_stop("parameters parsing: ABSOLUTE OUTPUT not yet implemented")
  elseif (samestring(str,"RELATIVE")) then
    defmrf%output = mrfdata_relative
  else
    call error_stop("parameters parsing: unknown or missing OUTPUT type for the moving reference frame")
  endif

  defmrf%rot_axis = v3d(0._krp, 0._krp, 1._krp)
  defmrf%rot_phi = 0._krp
  defmrf%trn_phi = 0._krp

  select case(defmrf%type)
  case(mrf_none)
    call print_info(7,"  no moving reference frame defined")

  case(mrf_trans_lin)
    call print_info(7,"  constant translational acceleration and/or velocity")
    call rpmgetkeyvalstr(pcour, "CENTER_ACCELERATION", str, "(0., 0., 0.)")
    defmrf%acceleration = v3d_of(str, info)
    call rpmgetkeyvalstr(pcour, "CENTER_VELOCITY", str, "(0., 0., 0.)")
    defmrf%velocity = v3d_of(str, info)

  case(mrf_trans_osc)
    call print_info(7,"  oscillating translation")
    call rpmgetkeyvalstr(pcour, "CENTER_ACCELERATION", str, "(0., 0., 0.)")
    defmrf%acceleration = v3d_of(str, info)
    call rpmgetkeyvalstr(pcour, "CENTER_VELOCITY", str, "(0., 0., 0.)")
    defmrf%velocity = v3d_of(str, info)
    call rpmgetkeyvalreal(pcour, "TRANS_AMPL",  defmrf%trn_ampl)
    call rpmgetkeyvalreal(pcour, "TRANS_PERIOD",  defmrf%trn_period)
    if (abs(defmrf%trn_period) < epsilon(1._krp)) then
      call error_stop("parameters parsing: invalid (null) TRANS_PERIOD for the linearly oscillating reference frame")
    endif
    call rpmgetkeyvalstr(pcour, "TRANS_DIR", str)
    defmrf%trn_dir = v3d_of(str, info)
    call rpmgetkeyvalreal(pcour, "TRANS_PHI",  defmrf%trn_phi, 0._krp)

  case(mrf_rot_cst)
    call print_info(7,"  constant rotational velocity")
    call rpmgetkeyvalstr(pcour, "CENTER", str, "(0., 0., 0.)")
    defmrf%center = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "AXIS", str, "(0., 0., 1.)")
    defmrf%rot_axis = v3d_of(str, info)     
    if (rpm_existkey(pcour, "OMEGA_RPM")) then
      call rpmgetkeyvalreal(pcour, "OMEGA_RPM",  omega_rpm)
      defmrf%omega = 2._krp*PIcst*omega_rpm/60._krp
    else
      call rpmgetkeyvalreal(pcour, "OMEGA",  defmrf%omega)
    endif

  case(mrf_rot_osc)
    call print_info(7,"  oscillating rotation")
    call rpmgetkeyvalstr(pcour, "CENTER", str, "(0., 0., 0.)")
    defmrf%center = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "AXIS", str, "(0., 0., 1.)")
    defmrf%rot_axis = v3d_of(str, info)     
    if (rpm_existkey(pcour, "OMEGA_RPM")) then
      call rpmgetkeyvalreal(pcour, "OMEGA_RPM",  omega_rpm)
      defmrf%omega = 2._krp*PIcst*omega_rpm/60._krp
    else
      call rpmgetkeyvalreal(pcour, "OMEGA",  defmrf%omega, 0._krp)
    endif
    call rpmgetkeyvalreal(pcour, "ROT_AMPL",  defmrf%rot_ampl)
    defmrf%rot_ampl = PIcst/180._krp*defmrf%rot_ampl
    call rpmgetkeyvalreal(pcour, "ROT_PERIOD", defmrf%rot_period)
    if (abs(defmrf%rot_period) < epsilon(1._krp)) then
      call error_stop("parameters parsing: invalid (null) ROT_PERIOD for the rotationally oscillating reference frame")
    endif
    call rpmgetkeyvalreal(pcour, "ROT_PHI",  defmrf%rot_phi, 0._krp)

  case(mrf_comb_osc)
    call print_info(7,"  combined oscillation (rotation and translation)")
    call rpmgetkeyvalstr(pcour, "CENTER", str, "(0., 0., 0.)")
    defmrf%center = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "AXIS", str, "(0., 0., 1.)")
    defmrf%rot_axis = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "CENTER_ACCELERATION", str, "(0., 0., 0.)")
    defmrf%acceleration = v3d_of(str, info)     
    call rpmgetkeyvalstr(pcour, "CENTER_VELOCITY", str, "(0., 0., 0.)")
    defmrf%velocity = v3d_of(str, info)     
    if (rpm_existkey(pcour, "OMEGA_RPM")) then
      call rpmgetkeyvalreal(pcour, "OMEGA_RPM",  omega_rpm)
      defmrf%omega = 2._krp*PIcst*omega_rpm/60._krp
    else
      call rpmgetkeyvalreal(pcour, "OMEGA",  defmrf%omega, 0._krp)
    endif
    call rpmgetkeyvalreal(pcour, "ROT_AMPL",  defmrf%rot_ampl)
    defmrf%rot_ampl = PIcst/180._krp*defmrf%rot_ampl
    call rpmgetkeyvalreal(pcour, "ROT_PERIOD", defmrf%rot_period)
    if (abs(defmrf%rot_period) < epsilon(1._krp)) then
      call error_stop("parameters parsing: invalid (null) ROT_PERIOD for the rotationally oscillating reference frame")
    endif
    call rpmgetkeyvalreal(pcour, "ROT_PHI",  defmrf%rot_phi, 0._krp)

    call rpmgetkeyvalreal(pcour, "TRANS_AMPL",  defmrf%trn_ampl)
    call rpmgetkeyvalreal(pcour, "TRANS_PERIOD",  defmrf%trn_period)
    if (abs(defmrf%trn_period) < epsilon(1._krp)) then
      call error_stop("parameters parsing: invalid (null) TRANS_PERIOD for the combined oscillating reference frame")
    endif
    call rpmgetkeyvalstr(pcour, "TRANS_DIR", str)
    defmrf%trn_dir = v3d_of(str, info)
    call rpmgetkeyvalreal(pcour, "TRANS_PHI",  defmrf%trn_phi, 0._krp)

  case default
    call error_stop("parameters parsing: unknown MRF type definition")
  endselect

  if (abs(defmrf%rot_axis) < epsilon(1._krp)) then
    call error_stop("parameters parsing: invalid (null) rotation AXIS for the rotating reference frame")
  else
    defmrf%rot_axis = defmrf%rot_axis/abs(defmrf%rot_axis)
  endif

case default
  call error_stop("parameters parsing: too many MRF blocks found")
endselect

endsubroutine def_mrf
!------------------------------------------------------------------------------!
! Changes history
!
! Dec  2010 : creation, MRF parameters parsing (A. Gardi & JG)
! May  2011 : addition of the combined oscillation case (mrf_rotlin_osc)
!------------------------------------------------------------------------------!

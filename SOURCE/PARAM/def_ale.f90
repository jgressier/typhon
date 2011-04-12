!------------------------------------------------------------------------------!
! Procedure : def_ale                     Auteur : A. Gardi
!                                         Date   : Mar 2011
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_ale(block, defale)

use RPM
use TYPHMAKE
use OUTPUT
use MATH
use MENU_ALE
use FCT_PARSER

implicit none

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_ale)  :: defale

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call init_defale(defale)

! looking for BLOCK:ALE --

pblock => block
call seekrpmblock(pblock, "ALE", 0, pcour, nkey)

select case(nkey)

case(0) 
  defale%type = ale_none

case(1)
  call print_info(5,"- Definition of Arbitrary Lagrangian Eulerian mesh movement (ALE)")

  call rpmgetkeyvalstr(pcour, "TYPE", str)

  defale%type = -1
  if (samestring(str,"NONE"))         defale%type = ale_none
  if (samestring(str,"GLOBAL"))       defale%type = ale_global
  if (samestring(str,"BODY"))         defale%type = ale_body

  select case(defale%type)
  
  case(ale_none)

  case(ale_global)  ! given global 3d mesh movement
    call print_info(5, "  . Given symbolic 3D GLOBAL MESH movement laws found, enabling ALE")
    call rpmgetkeyvalstr(pcour, "MESH_MOVEMENT_X", str, "0.")
    call convert_to_funct(str, defale%movement_x, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand MESH_MOVEMENT_X data (ALE - given symbolic 3D GLOBAL MESH movement)")
    call rpmgetkeyvalstr(pcour, "MESH_MOVEMENT_Y", str, "0.")
    call convert_to_funct(str, defale%movement_y, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand MESH_MOVEMENT_Y data (ALE - given symbolic 3D GLOBAL MESH movement)")
    call rpmgetkeyvalstr(pcour, "MESH_MOVEMENT_Z", str, "0.")
    call convert_to_funct(str, defale%movement_z, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand MESH_MOVEMENT_Z data (ALE - given symbolic 3D GLOBAL MESH movement)")

  case(ale_body)   ! given BODY 3d+theta movement
    call print_info(5, "  . Given symbolic 3D+theta BODY movement laws found, enabling ALE")
    call rpmgetkeyvalstr(pcour, "MOVING_BODY", str)
    defale%moving_body = str
    call rpmgetkeyvalstr(pcour, "BODY_MOVEMENT_X", str, "0.")
    call convert_to_funct(str, defale%movement_x, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand BODY_MOVEMENT_X data (ALE - given symbolic 3D BODY movement)")
    call rpmgetkeyvalstr(pcour, "BODY_MOVEMENT_Y", str, "0.")
    call convert_to_funct(str, defale%movement_y, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand BODY_MOVEMENT_Y data (ALE - given symbolic 3D BODY movement)")
    call rpmgetkeyvalstr(pcour, "BODY_MOVEMENT_Z", str, "0.")
    call convert_to_funct(str, defale%movement_z, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand BODY_MOVEMENT_Z data (ALE - given symbolic 3D BODY movement)")
    call rpmgetkeyvalstr(pcour, "BODY_MOVEMENT_THETA", str, "0.")
    call convert_to_funct(str, defale%movement_theta, info)
    if (info /= 0) &
      call erreur("parsing parameters","unable to understand BODY_MOVEMENT_THETA data (ALE - given symbolic 3D BODY movement)")
    if (rpm_existkey(pcour,"BODY_CENTRE")) then
      call rpmgetkeyvalstr(pcour, "BODY_CENTRE", str)
      defale%body_centre = v3d_of(str, info)
      if (info /= 0) &
        call erreur("parsing parameters","unable to understand BODY_CENTRE data (ALE - given symbolic 3D BODY movement)")
    endif
    call rpmgetkeyvalreal(pcour, "INTERP_MINRADIUS", defale%body_maxradius, -1._krp)
    call rpmgetkeyvalreal(pcour, "INTERP_MAXRADIUS", defale%closest_boundary, -1._krp)

  case default
    call error_stop("parameters parsing: unknown ALE mesh movement type definition")
  endselect

case default
  call error_stop("parameters parsing: too many ALE blocks found")
endselect

endsubroutine def_ale
!------------------------------------------------------------------------------!
! Changes history
!
! Mar 2011 : creation, ALE parameters parsing (A. Gardi & JG)
!------------------------------------------------------------------------------!

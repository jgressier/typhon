!------------------------------------------------------------------------------!
! Procedure : def_mesh                              Authors : J. Gressier
!                                                   Created : November 2002
! Fonction                                          Modif  : (see history)
!   Parse main file parameters / Mesh definition
!
!------------------------------------------------------------------------------!
subroutine def_mesh(block, defmesh)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use STRING
use MATH
use GEO3D
use MENU_MESH
use MENU_GEN

implicit none

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_mesh) :: defmesh

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i, ip, info
real(krp)                :: x
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- mesh definition")

! -- Recherche du BLOCK:MESH

pblock => block
call seekrpmblock(pblock, "MESH", 0, pcour, nkey)

if (nkey /= 1) call erreur("parameter parsing", &
                           "bloc MESH inexistant ou surnumeraire")

! -- lecture du format

call rpmgetkeyvalstr(pcour, "FORMAT", str)
defmesh%format = cnull

if (samestring(str,"CGNS"))    defmesh%format = fmt_CGNS
if (samestring(str,"TYPHMSH")) defmesh%format = fmt_TYPHMSH
if (defmesh%format == cnull) call erreur("lecture de menu","format de maillage inconnu")

defmesh%icgnsbase = 1
defmesh%icgnszone = 1

! -- lecture du nom de fichier

call rpmgetkeyvalstr(pcour, "FILE", str)
defmesh%filename = str

! -- read scale factor (default 1.)

call rpmgetkeyvalreal(pcour, "SCALE", defmesh%scale, 1._krp)

! -- read split method --

call rpmgetkeyvalstr(pcour, "SPLIT", str, "NONE")
if (samestring(str,"NONE"))      defmesh%splitmesh = split_none
if (samestring(str,"SVM2TRI"))   defmesh%splitmesh = split_svm2tri
if (samestring(str,"SVM2QUAD"))  defmesh%splitmesh = split_svm2quad
if (samestring(str,"SVM3WANG"))  defmesh%splitmesh = split_svm3wang
if (samestring(str,"SVM3KRIS"))  defmesh%splitmesh = split_svm3kris
if (samestring(str,"SVM3KRIS2")) defmesh%splitmesh = split_svm3kris2
if (samestring(str,"SVM4WANG"))  defmesh%splitmesh = split_svm4wang
if (samestring(str,"SVM4KRIS"))  defmesh%splitmesh = split_svm4kris
if (samestring(str,"SVM4KRIS2")) defmesh%splitmesh = split_svm4kris2

select case(defmesh%splitmesh)
case(split_none)
  ! nothing to write
case(split_svm2quad)
  call print_info(20, "  . split mesh : SVM2 based (3 quads)")
case(split_svm3wang)
  call print_info(20, "  . split mesh : SVM3 WANG ORIGINAL")
case(split_svm3kris)
  call print_info(20, "  . split mesh : SVM3 KRIS OPTIMISED")
case(split_svm3kris2)
  call print_info(20, "  . split mesh : SVM3 KRIS OPTIMISED 2")
case(split_svm4wang)
  call print_info(20, "  . split mesh : SVM4 WANG ORIGINAL")
case(split_svm4kris)
  call print_info(20, "  . split mesh : SVM4 KRIS OPTIMISED")
case(split_svm4kris2)
  call print_info(20, "  . split mesh : SVM4 KRIS OPTIMISED 2")
case default
  call erreur("Development", "unknown splitmesh parameter (def_mesh)")
endselect

! ----------------------------------------------------------------------------
! Periodicity BLOCKS

pblock => block
call seekrpmblock(pblock, "PERIODICITY", 0, pcour, defmesh%nperiodicity)

if (defmesh%nperiodicity >= 1) then

  allocate(defmesh%periodicity(defmesh%nperiodicity))

  do ip = 1, defmesh%nperiodicity

    call rpmgetkeyvalstr(pcour, "NAME", str)
    defmesh%periodicity(ip)%name = trim(str)

    call rpmgetkeyvalstr(pcour, "TYPE", str)

    defmesh%periodicity(ip)%type = inull
    if (samestring(str, "TRANSLATION" )) defmesh%periodicity(ip)%type = per_trans
    if (samestring(str, "ROTATION" ))    defmesh%periodicity(ip)%type = per_rot

    call print_info(20, "  . periodicity definition, type "//str(1:12)//": "//trim(defmesh%periodicity(ip)%name))

    select case(defmesh%periodicity(ip)%type)

    case(per_trans)
      call rpmgetkeyvalstr(pcour, "TRANSLATION", str)
      defmesh%periodicity(ip)%distance = v3d_of(str, info)  

    case(per_rot)
      call rpmgetkeyvalstr (pcour, "ROTATION_CENTER", str, "(0., 0., 0.)")
      defmesh%periodicity(ip)%origin = v3d_of(str, info)  
      call rpmgetkeyvalstr (pcour, "ROTATION_AXIS", str)
      defmesh%periodicity(ip)%axis = v3d_of(str, info)  
      defmesh%periodicity(ip)%axis = defmesh%periodicity(ip)%axis / abs(defmesh%periodicity(ip)%axis)
      if (rpm_existkey(pblock, "ROTATION_ANGLE")) then
       call rpmgetkeyvalreal(pcour, "ROTATION_ANGLE", x)
       defmesh%periodicity(ip)%angle = x/180._krp*pi
      else
       call rpmgetkeyvalreal(pcour, "ROTATION_NUMBER", x)
       defmesh%periodicity(ip)%angle = 2._krp*pi/x
      endif
    case default
      call erreur("parameters parsing","periodicity model")
    endselect

  enddo
endif


endsubroutine def_mesh

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! fev  2004 : lecture de format TYPHMSH (format interne)
! sept 2005 : add scale factor
! oct  2007 : splitting option
!------------------------------------------------------------------------------!

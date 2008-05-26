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
use MENU_MESH
use MENU_GEN

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_mesh) :: defmesh

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

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

! -- lecture du nom de fichier

call rpmgetkeyvalstr(pcour, "FILE", str)
defmesh%fichier = str

! -- read scale factor (default 1.)

call rpmgetkeyvalreal(pcour, "SCALE", defmesh%scale, 1._krp)

! -- read split method --

call rpmgetkeyvalstr(pcour, "SPLIT", str, "NONE")
if (samestring(str,"NONE"))      defmesh%splitmesh = split_none
if (samestring(str,"SVM2TRI"))   defmesh%splitmesh = split_svm2tri
if (samestring(str,"SVM2QUAD"))  defmesh%splitmesh = split_svm2quad
if (samestring(str,"SVM3WANG"))  defmesh%splitmesh = split_svm3wang
if (samestring(str,"SVM3KRIS"))  defmesh%splitmesh = split_svm3kris
if (samestring(str,"SVM3KRIS2"))  defmesh%splitmesh = split_svm3kris2

select case(defmesh%splitmesh)
case(split_none)
  ! nothing to write
case(split_svm2quad)
  call print_info(20, "  . split mesh : SVM based (3 quads)")
case(split_svm3wang)
  call print_info(20, "  . split mesh : SVM WANG ORIGINAL")
case(split_svm3kris)
  call print_info(20, "  . split mesh : SVM KRIS OPTIMISED")
case(split_svm3kris2)
  call print_info(20, "  . split mesh : SVM KRIS OPTIMISED 2")
case default
  call erreur("Development", "unknown splitmesh parameter (def_mesh)")
endselect

endsubroutine def_mesh

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! fev  2004 : lecture de format TYPHMSH (format interne)
! sept 2005 : add scale factor
! oct  2007 : splitting option
!------------------------------------------------------------------------------!

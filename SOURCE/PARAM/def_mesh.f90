!------------------------------------------------------------------------------!
! Procedure : def_mesh                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_mesh(block, defmesh)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_MESH

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_mesh) :: defmesh

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition du maillage")

! -- Recherche du BLOCK:MESH

pblock => block
call seekrpmblock(pblock, "MESH", 0, pcour, nkey)

! call printrpmblock(6, pblock, .true.)  !! DEBUG
! call printrpmblock(6, pcour, .true.)   !! DEBUG
if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MESH inexistant ou surnuméraire")

! -- lecture du format

call rpmgetkeyvalstr(pcour, "FORMAT", str)
defmesh%format = cnull

if (samestring(str,"CGNS"))  defmesh%format = fmt_CGNS
if (defmesh%format == cnull) call erreur("lecture de menu","format de maillage inconnu")

! -- lecture du nom de fichier

call rpmgetkeyvalstr(pcour, "FILE", str)
defmesh%fichier = str


endsubroutine def_mesh

!------------------------------------------------------------------------------!
! Procedure : def_output                  Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_output(block, world)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MENU_GEN

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(st_world) :: world

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey, nkey2    ! nombre de clefs
integer                  :: i, io
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(2,"* Définition des sorties résultats")

! -- Recherche du BLOCK:OUTPUT

pblock => block
call seekrpmblock(pblock, "OUTPUT", 0, pcour, nkey)
!
!if (nkey /= 1) call erreur("lecture de menu", &
!                           "bloc OUTPUT inexistant ou surnuméraire")

world%noutput = nkey !DEV2602    world%noutput = 1
allocate(world%output(world%noutput)) !DEV2602 allocate(world%output(1))

do io = 1, world%noutput !DEV2602 
  ! -- lecture du format
  call seekrpmblock(pblock, "OUTPUT", io, pcour, nkey) 
  call rpmgetkeyvalstr(pcour, "FORMAT", str)
  world%output(io)%format = cnull

  if (samestring(str,"TECPLOT")) world%output(io)%format = fmt_TECPLOT
  if (samestring(str,"VIGIE"))   world%output(io)%format = fmt_VIGIE
  if (world%output(io)%format == cnull) call erreur("lecture de menu","format de fichier inconnu")

  ! -- lecture du nom de fichier

  call rpmgetkeyvalstr(pcour, "FILE", str)
  world%output(io)%fichier = str

  ! -- Détermination du type de sortie : aux noeuds du maillage 
  !    ou au centre des cellules (par défaut au centre)

  call rpmgetkeyvalstr(pcour, "TYPE", str, "CENTER")

  if (samestring(str,"NODE"))   world%output(io)%type = outp_NODE
  if (samestring(str,"CENTER")) world%output(io)%type = outp_CENTER
  if (samestring(str,"CORRECTION")) world%output(io)%type = outp_COR !DEV602
  if (samestring(str,"FLUX"))   world%output(io)%type = outp_FLUX !DEV2602
  if (samestring(str,"WALLTEMP")) world%output(io)%type = outp_TEMPINTER !DEV1404

enddo

!-----------------------------
endsubroutine def_output
!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : création de la procédure
! oct  2003 : choix des sorties NODE ou CENTER pour Tecplot
!------------------------------------------------------------------------------!

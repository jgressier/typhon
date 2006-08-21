!------------------------------------------------------------------------------!
! Procedure : def_output                  Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
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

! -- Declaration des entrees --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(st_world) :: world

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey, nkey2    ! nombre de clefs
integer                  :: i, io
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(2,"* Definition des sorties resultats")

! -- Recherche du BLOCK:OUTPUT

pblock => block
call seekrpmblock(pblock, "OUTPUT", 0, pcour, nkey)
!
!if (nkey /= 1) call erreur("lecture de menu", &
!                           "bloc OUTPUT inexistant ou surnumeraire")

world%noutput = nkey !DEV2602    world%noutput = 1
allocate(world%output(world%noutput)) !DEV2602 allocate(world%output(1))

do io = 1, world%noutput !DEV2602 
  ! -- lecture du format
  call seekrpmblock(pblock, "OUTPUT", io, pcour, nkey) 
  call rpmgetkeyvalstr(pcour, "FORMAT", str)
  world%output(io)%format = cnull

  if (samestring(str,"TECPLOT"))    world%output(io)%format = fmt_TECPLOT
  if (samestring(str,"VIGIE"))      world%output(io)%format = fmt_VIGIE
  if (samestring(str,"VTK"))        world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-ASCII"))  world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-BIN"))    world%output(io)%format = fmt_VTKBIN
  if (samestring(str,"VTK-BINARY")) world%output(io)%format = fmt_VTKBIN
  if (world%output(io)%format == cnull) call erreur("lecture de menu","format de fichier inconnu")

  ! -- lecture du nom de fichier

  call rpmgetkeyvalstr(pcour, "FILE", str)
  world%output(io)%fichier = str

  ! -- Determination du type de sortie : aux noeuds du maillage 
  !    ou au centre des cellules (par defaut au centre)

  call rpmgetkeyvalstr(pcour, "TYPE", str, "CENTER")

  if (samestring(str,"NODE"))       world%output(io)%type = outp_NODE
  if (samestring(str,"CENTER"))     world%output(io)%type = outp_CENTER
  if (samestring(str,"CORRECTION")) world%output(io)%type = outp_COR
  if (samestring(str,"FLUX"))       world%output(io)%type = outp_FLUX
  if (samestring(str,"WALLTEMP"))   world%output(io)%type = outp_TEMPINTER

  select case(world%output(io)%type)
  case(outp_NODE, outp_CENTER)
    call rpmgetkeyvalint(pcour, "PERIOD",  world%output(io)%period, 0)
  case(outp_TEMPINTER, outp_COR, outp_FLUX)
    call rpmgetkeyvalint(pcour, "PERIOD",  world%output(io)%period, 1)
  case default
    call erreur("internal error (def_output)", "unknown parameter")
  endselect

enddo

!-----------------------------
endsubroutine def_output
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! oct  2003 : choix des sorties NODE ou CENTER pour Tecplot
!------------------------------------------------------------------------------!

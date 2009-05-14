!------------------------------------------------------------------------------!
! Procedure : def_output 
!         
! Fonction
!   Reading parameters for output
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

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(st_world) :: world

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey, nkey2    ! nombre de clefs
integer                  :: i, io, ic
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(2,"* Definition of output parameters")

! -- Recherche du BLOCK:OUTPUT

pblock => block
call seekrpmblock(pblock, "OUTPUT", 0, pcour, nkey)
!
!if (nkey /= 1) call erreur("lecture de menu", &
!                           "bloc OUTPUT inexistant ou surnumeraire")

world%noutput = nkey !DEV2602    world%noutput = 1
allocate(world%output(world%noutput)) !DEV2602 allocate(world%output(1))

do io = 1, world%noutput 

  ! -- lecture du format

  call seekrpmblock(pblock, "OUTPUT", io, pcour, nkey)

  call rpmgetkeyvalstr(pcour, "FORMAT", str)
  world%output(io)%format = cnull

  if (samestring(str,"CGNS"))        world%output(io)%format = fmt_CGNS
  if (samestring(str,"CGNS-LINKED")) world%output(io)%format = fmt_CGNS_linked
  if (samestring(str,"TECPLOT"))     world%output(io)%format = fmt_TECPLOT
  if (samestring(str,"VIGIE"))       world%output(io)%format = fmt_VIGIE
  if (samestring(str,"VTK"))         world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-ASCII"))   world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-BIN"))     world%output(io)%format = fmt_VTKBIN
  if (samestring(str,"VTK-BINARY"))  world%output(io)%format = fmt_VTKBIN
  if (world%output(io)%format == cnull) call erreur("parameter reading","unknown format <"//trim(str)//">")

  call print_info(10,"  . file"//strof(io,3)//": "//trim(str)//" format")

  ! -- filename or basename

  call rpmgetkeyvalstr(pcour, "FILE", str)
  select case(world%output(io)%format)
  case(fmt_CGNS, fmt_CGNS_linked)
    ic = index(str, ".cgns")
    if (ic == len(trim(str))-4) str = str(1:ic-1)//"     "
  case(fmt_VTK, fmt_VTKBIN)
    ic = index(str, ".vtk")
    if ((ic /= 0).and.(ic == len(trim(str))-3)) str = str(1:ic-1)//"    "
  case(fmt_TECPLOT)
    ic = index(str, ".dat")
    if ((ic /= 0).and.(ic == len(trim(str))-3)) str = str(1:ic-1)//"    "
  case default
    call erreur("internal error (def_output)", "unknown format")
  endselect
  world%output(io)%filename = str

  ! --- type of output --- 

  call rpmgetkeyvalstr(pcour, "TYPE", str, "CELL")

  world%output(io)%dataset = inull
  if (samestring(str,"NODE"))       world%output(io)%dataset = dataset_node
  if (samestring(str,"CENTER"))     world%output(io)%dataset = dataset_cell
  if (samestring(str,"CELL"))       world%output(io)%dataset = dataset_cell
  if (samestring(str,"SVCELL"))     world%output(io)%dataset = dataset_svcell
  if (samestring(str,"BOCO"))       world%output(io)%dataset = dataset_bococell
  if (samestring(str,"BOCOCELL"))   world%output(io)%dataset = dataset_bococell
  if (samestring(str,"BOCONODE"))   world%output(io)%dataset = dataset_boconode
  if (world%output(io)%dataset == inull) call erreur("parameter reading","unknown dataset option")

  call print_info(20,"    data set type:"//trim(str))

  call rpmgetkeyvalint(pcour, "PERIOD",  world%output(io)%period, huge(world%output(io)%period))


enddo

!-----------------------------
endsubroutine def_output
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! oct  2003 : choix des sorties NODE ou CENTER pour Tecplot
! fev  2007 : correction du nom de fichier pour VTK[BIN] et TECPLOT
! fev  2007 : Traduction en anglais
! Mar  2009 : CGNS file
!------------------------------------------------------------------------------!

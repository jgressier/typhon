!------------------------------------------------------------------------------!
! Procedure : def_output
!
! Fonction
!   Reading parameters for output
!
!------------------------------------------------------------------------------!
subroutine def_output(block, world)

use RPM
use OUTPUT
use VARCOM
use MODWORLD
use MENU_GEN
use MESHMRF

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
character(len=4)         :: suffix         ! suffix string

! -- BODY --

call print_info(2,"* Definition of output parameters")

! -- Recherche du BLOCK:OUTPUT

pblock => block
call seekrpmblock(pblock, "OUTPUT", 0, pcour, nkey)

world%noutput = nkey+1                 ! add automatic restart
allocate(world%output(world%noutput))

!-----------------------------------------------------------
! define automatic restart file

io = world%noutput
world%output(io)%format    = fmt_TYPHON
world%output(io)%basename  = "restart"  ! '.tys'
world%output(io)%dataset   = dataset_cell
world%output(io)%meshlevel = meshlevel_legacy
world%output(io)%meshdef   = mesh_full
world%output(io)%savedmesh = .false.
world%output(io)%period    = huge(world%output(io)%period)
world%output(io)%refframe  = mrfdata_relative

!-----------------------------------------------------------
! parse only BLOCK definition

do io = 1, nkey   ! parse only BLOCK definition

  ! -- lecture du format

  call seekrpmblock(pblock, "OUTPUT", io, pcour, nkey)

  call rpmgetkeyvalstr(pcour, "FORMAT", str)
  world%output(io)%format = cnull

  if (samestring(str,"TYPHON"))      world%output(io)%format = fmt_TYPHON
#ifdef CGNS
  if (samestring(str,"CGNS"))        world%output(io)%format = fmt_CGNS
  if (samestring(str,"CGNS-LINKED")) world%output(io)%format = fmt_CGNS_linked
#else /*CGNS*/
  if (samestring(str,"CGNS") .or. &
      samestring(str,"CGNS-LINKED")) then
    call error_stop("Parameter reading: CGNS format was not activated at configure time")
  endif
#endif/*CGNS*/
  if (samestring(str,"TECPLOT"))     world%output(io)%format = fmt_TECPLOT
  if (samestring(str,"VIGIE"))       world%output(io)%format = fmt_VIGIE
  if (samestring(str,"VTK"))         world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-ASCII"))   world%output(io)%format = fmt_VTK
  if (samestring(str,"VTK-BIN"))     world%output(io)%format = fmt_VTKBIN
  if (samestring(str,"VTK-BINARY"))  world%output(io)%format = fmt_VTKBIN
  if (world%output(io)%format == cnull) call error_stop("Parameter reading: unknown format <"//trim(str)//">")

  call print_info(10,"  . file"//strofr(io,3)//": "//trim(str)//" format")

  ! -- filename or basename

  call rpmgetkeyvalstr(pcour, "FILE", str)
  select case(world%output(io)%format)
  case(fmt_TYPHON)
    suffix = xtyext_sol
#ifdef CGNS
  case(fmt_CGNS, fmt_CGNS_linked)
    suffix = "cgns"
#endif/*CGNS*/
  case(fmt_VTK, fmt_VTKBIN)
    suffix = "vtk"
  case(fmt_TECPLOT)
    suffix = "dat"
  case default
    call error_stop("Internal error (def_output): unknown format")
  endselect
  world%output(io)%basename = basename(str, suffix)

  ! --- type of mesh output ---

  call rpmgetkeyvalstr(pcour, "MESH", str, "SHARED")    ! default is shared, even not possible with all formats

  world%output(io)%meshdef = inull
  if (samestring(str,"FULL"))       world%output(io)%meshdef = mesh_full
  if (samestring(str,"SHARED"))     world%output(io)%meshdef = mesh_shared
  if (samestring(str,"SHAREDCON"))  world%output(io)%meshdef = mesh_sharedcon
  if (world%output(io)%meshdef == inull) call error_stop("Parameter reading: unknown dataset option (MESH key)")

  world%output(io)%savedmesh = .false.
  call print_info(20,"    mesh definition type: "//trim(str))

  ! --- type of mesh output ---

  call rpmgetkeyvalstr(pcour, "MESHLEVEL", str, "CURRENT")    ! default is current computed mesh (except restart file)

  world%output(io)%meshlevel = inull
  if (samestring(str,"LEGACY"))     world%output(io)%meshlevel = meshlevel_legacy
  if (samestring(str,"CURRENT"))    world%output(io)%meshlevel = meshlevel_current
  if (world%output(io)%meshlevel == inull) call error_stop("Parameter reading: unknown dataset option (MESHLEVEL key)")

  call print_info(20,"    mesh level output   : "//trim(str))

  ! --- output period ---

  call rpmgetkeyvalint(pcour, "PERIOD",  world%output(io)%period, huge(world%output(io)%period))

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
  if (world%output(io)%dataset == inull) call error_stop("Parameter reading: unknown dataset option")

  call print_info(20,"           data set type:"//trim(str))

  call rpmgetkeyvalint(pcour, "PERIOD",  world%output(io)%period, huge(world%output(io)%period))

  ! --- reference frame ---

  call rpmgetkeyvalstr(pcour, "REFFRAME", str, "RELATIVE")

  world%output(io)%refframe = inull
  if (samestring(str,"RELATIVE"))  world%output(io)%refframe = mrfdata_relative
  if (samestring(str,"ABSOLUTE"))  world%output(io)%refframe = mrfdata_absolute
  if (world%output(io)%refframe == inull) call error_stop("Parameter reading: unknown dataset option")

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
! Mar  2011 : Definition of reference frame
! May  2011 : Definition of mesh sharing
!------------------------------------------------------------------------------!

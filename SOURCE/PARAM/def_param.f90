!------------------------------------------------------------------------------!
! Procedure : def_param
!
! Fonction
!   Lecture des menus et traitement pour definition des parametres
!
!------------------------------------------------------------------------------!
subroutine def_param(lworld)

use RPM        ! librairie de blocs RPM pour la lecture des parametres
use TYPHMAKE   ! definition de la precision
use IO_UNIT    !
use OUTPUT     ! definition des unites de sortie
use MODWORLD   ! definition des donnees globales
use FTNARGS

implicit none

! -- INPUTS --

! -- OUTPUTS --
type(st_world) :: lworld

! -- Private Data --
type(rpmblock), pointer :: firstblock
integer                 :: info         ! etat de l'ouverture de fichier
character(len=longname) :: mainfile
integer                 :: uf_menu, iarg, nargs
character(len=256)      :: str_opt, str_val
logical, parameter      :: lincr = .TRUE.


! -- BODY --

!---------------------------------------------------------
! command line parsing
!---------------------------------------------------------

lworld%prj%action = act_compute
mainfile = "main.rpm"

nargs    = command_argument_count()
iarg     = 1

do while (iarg <= nargs)
  call read_command_argument(iarg, str_opt, lincr)
  select case(str_opt)
  case ("-h","--help")
    call print_help()
    call error_stop("")
  case ("-i")
    print*,iarg,nargs
    if (iarg>nargs) call error_stop("missing argument after '"//trim(str_opt)//"'")
    call read_command_argument(iarg, mainfile, lincr)
    call print_info(1,"option -i: read "//trim(mainfile)//" file")
  case ("--analyze")
    lworld%prj%action = act_analyze
    call print_info(1,"option: ANALYZE mode")
  case ("--restart")
    lworld%prj%action = act_restart
    call print_info(1,"option: RESTART mode")
  case default
    call print_help()
    call error_stop("unknown argument ("//trim(str_opt)//")")
  endselect
enddo

!---------------------------------------------------------
! Input file
!---------------------------------------------------------

call print_etape("> PARAMETERS reading : main file ("//trim(mainfile)//")")

uf_menu = getnew_io_unit()

open(unit=uf_menu, file=trim(mainfile), form="formatted", action="read", iostat=info)
if (info /= 0) call error_stop("reading input file "//trim(mainfile)//" not found or readable")

!allocate(firstblock)
!nullify(firstblock)

call readrpmblock(uf_menu, uf_log, 1, firstblock) ! Lecture du fichier de parametres

call close_io_unit(uf_menu)

!call printrpmblock(6, firstblock, .false.)

!---------------------------------------------------------
! Traitement des parametres lus et configuration WORLD
!---------------------------------------------------------

call print_etape("> PARAMETERS : parsing and initialization")

call trait_param(firstblock, lworld)

!---------------------------------------------------------

call dealloc_rpmblock(firstblock)        ! Desallocation de la liste RPM

contains

subroutine print_help()
  call print_info(1,"")
  call print_info(1,"command line options:")
  call print_info(1,"  -h|--help  : print this help")
  call print_info(1,"  -i file    : define computation parameters (default: main.rpm)")
  call print_info(1,"  --restart  : read restart.tys mesh and solution")
  call print_info(1,"  --analyze  : pre-processing only and save boundary nodes")
  call print_info(1,"")
endsubroutine

endsubroutine def_param
!------------------------------------------------------------------------------!
! change history
!
! July 2003: created
!------------------------------------------------------------------------------!

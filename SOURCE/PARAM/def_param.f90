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

implicit none

! -- INPUTS --

! -- OUTPUTS --
type(st_world) :: lworld

! -- Private Data --
type(rpmblock), pointer :: firstblock
integer                 :: info         ! etat de l'ouverture de fichier
character(len=longname) :: fic
integer                 :: uf_menu

! -- BODY --

!---------------------------------------------------------
! Lecture des parametres
!---------------------------------------------------------

call print_etape("> PARAMETERS reading : main file (main.rpm)")

fic     = "main.rpm"
uf_menu = getnew_io_unit()

open(unit=uf_menu, file=trim(fic), form="formatted", action="read", iostat=info)
if (info /= 0) call erreur("Lecture du menu","fichier "//trim(fic)// &
                           " introuvable ou interdit en lecture")

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


endsubroutine def_param
!------------------------------------------------------------------------------!
! change history
!
! July 2003: created
!------------------------------------------------------------------------------!

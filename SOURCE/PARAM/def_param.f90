!------------------------------------------------------------------------------!
! Procedure : def_param                   Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : Novembre 2002
!   Lecture des menus et traitement pour definition des parametres
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine def_param(lworld)

use RPM        ! librairie de blocs RPM pour la lecture des parametres
use TYPHMAKE   ! definition de la precision
use OUTPUT     ! definition des unites de sortie
use MODWORLD   ! definition des donnees globales

implicit none

! -- Declaration des entrees --

! -- Declaration des sorties --
type(st_world) :: lworld

! -- Declaration des variables internes --
type(rpmblock), pointer :: firstblock
integer                 :: info         ! etat de l'ouverture de fichier
character(len=strlen)   :: fic

! -- Debut de la procedure --

!---------------------------------------------------------
! Lecture des parametres
!---------------------------------------------------------

call print_etape("> LECTURE : fichier menu principal")

fic = "main.rpm"

open(unit=uf_menu, file=trim(fic), form="formatted", action="read", iostat=info)
if (info /= 0) call erreur("Lecture du menu","fichier "//trim(fic)// &
                           " introuvable ou interdit en lecture")

!allocate(firstblock)
!nullify(firstblock)

call readrpmblock(uf_menu, uf_log, 1, firstblock) ! Lecture du fichier de parametres
close(uf_menu)

!call printrpmblock(6, firstblock, .false.)

!---------------------------------------------------------
! Traitement des parametres lus et configuration WORLD
!---------------------------------------------------------

call print_etape("> PARAMETRES : traitement et initialisation")

call trait_param(firstblock, lworld)

!---------------------------------------------------------

call dealloc_rpmblock(firstblock)        ! Desallocation de la liste RPM


endsubroutine def_param

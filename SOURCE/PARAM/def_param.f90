!------------------------------------------------------------------------------!
! Procedure : def_param                   Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : Novembre 2002
!   Lecture des menus et traitement pour définition des paramètres
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine def_param(lworld)

use RPM        ! librairie de blocs RPM pour la lecture des paramètres
use TYPHMAKE   ! définition de la précision
use OUTPUT     ! définition des unités de sortie
use MODWORLD   ! définition des données globales

implicit none

! -- Declaration des entrées --

! -- Declaration des sorties --
type(st_world) :: lworld

! -- Declaration des variables internes --
type(rpmblock), pointer :: firstblock
integer                 :: info         ! etat de l'ouverture de fichier
character(len=strlen)   :: fic

! -- Debut de la procedure --

call print_etape("> LECTURE : fichier menu principal")

fic = "main.rpm"

open(unit=uf_menu, file=trim(fic), iostat=info)
if (info /= 0) call erreur("Lecture du menu","fichier "//trim(fic)// &
                           " introuvable ou interdit en lecture")

allocate(firstblock)
nullify(firstblock)   ! nécessaire pour le bon fonctionnement de readrpmblock

call readrpmblock(uf_menu, uf_log, 1, firstblock) ! Lecture du fichier de paramètres
close(uf_menu)

!call printrpmblock(6,firstblock,.false.)

call print_etape("> PARAMETRES : traitement et initialisation")

call trait_param(firstblock, lworld)

call dealloc_rpmblock(firstblock)            ! Désallocation de la liste RPM


endsubroutine def_param

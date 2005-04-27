!------------------------------------------------------------------------------!
! Procedure : def_project                 Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : cf historique
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_project(block, prj)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_GEN

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_project) :: prj

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey, nkey2    ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(2,"* Definition du projet")

! -- Recherche du BLOCK:PROJECT 

pblock => block
call seekrpmblock(pblock, "PROJECT", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc PROJECT inexistant ou surnumeraire")

! -- Determination du nombre de zone (1 par defaut)

call rpmgetkeyvalint(pcour, "NZONE", prj%nzone, 1)
write(str_w,*) ". nombre de zones du projet : ",prj%nzone
call print_info(8,adjustl(str_w))

! -- Determination du nombre de couplages entre zones (0 par defaut)

call rpmgetkeyvalint(pcour, "NCOUPLING", prj%ncoupling, 0)
write(str_w,*) ". nombre de couplages dans le projet : ",prj%ncoupling
call print_info(8,adjustl(str_w))

! -- Determination du type de repere

call rpmgetkeyvalstr(pcour, "COORD", str)

prj%typ_coord = ' '
if (samestring(str, "2D" ))     prj%typ_coord = c2dplan
if (samestring(str, "2DPLAN" )) prj%typ_coord = c2dplan
if (samestring(str, "2DAXI"))   prj%typ_coord = c2daxi
if (samestring(str, "3D"))      prj%typ_coord = c3dgen

select case(prj%typ_coord)
case(c2dplan)
  call print_info(10,"repere 2d plan")
case(c2daxi) 
  call print_info(10,"repere 2d axisymetrique")
  ! DEV : lecture de la position de l'axe
case(c3dgen) 
  call print_info(10,"repere 3d general")
case default
  call erreur("lecture de menu","type de repere inconnu")
endselect

! -- Determination du type d'evolution temporelle

call rpmgetkeyvalstr(pcour, "TIME", str)

prj%typ_temps = ' '
if (samestring(str, "STEADY" ))   prj%typ_temps = stationnaire
if (samestring(str, "UNSTEADY" )) prj%typ_temps = instationnaire
if (samestring(str, "PERIODIC"))  prj%typ_temps = periodique

select case(prj%typ_temps)

case(stationnaire) ! Evolution pseudo-instationnaire
  call print_info(10,"calcul stationnaire (convergence pseudo-instationnaire)")
  if (.not.(rpm_existkey(pcour,"RESIDUALS").or.rpm_existkey(pcour,"NCYCLE"))) then
    call erreur("lecture de menu","parametre RESIDUALS ou NCYCLE manquant")
  endif
  call rpmgetkeyvalreal(pcour, "RESIDUALS", prj%residumax)
  call rpmgetkeyvalint (pcour, "NCYCLE",    prj%ncycle, 1)
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS

  
case(instationnaire) ! Evolution instationnaire
  call print_info(10,"calcul instationnaire")
  call rpmgetkeyvalreal(pcour, "DURATION", prj%duree)
  call rpmgetkeyvalreal(pcour, "BASETIME", prj%dtbase)
  prj%tpsbase = prj%dtbase
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS

case(periodique) ! Evolution periodique
  call print_info(10,"calcul cyclique")
  call rpmgetkeyvalreal(pcour, "PERIOD", prj%duree)
  call rpmgetkeyvalint (pcour, "NCYCLE", prj%ncycle)
  prj%dtbase = prj%duree / prj%ncycle
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS
  call erreur("Developpement","calcul periodique non implemente")

case default
  call erreur("lecture de menu","type d'integration temporelle inconnu")
endselect



endsubroutine def_project

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation de la procedure
! juin 2003 : ajout de la definition des couplages
! sept 2003 : parametres pour le calcul stationnaire
!------------------------------------------------------------------------------!



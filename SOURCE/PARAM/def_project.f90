!------------------------------------------------------------------------------!
! Procedure : def_project                              Authors : J. Gressier
!                                                      Created : November 2002
! Fonction                                             Modif  : cf history
!   Parse main file parameters / main parameters for the project
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

call print_info(2,"* Project definition")

! -- Recherche du BLOCK:PROJECT 

pblock => block
call seekrpmblock(pblock, "PROJECT", 0, pcour, nkey)

if (nkey /= 1) call erreur("parameter parsing", &
                           "bloc PROJECT inexistant ou surnumeraire")

! -- Determination du nombre de zone (1 par defaut)

call rpmgetkeyvalint(pcour, "NZONE", prj%nzone, 1)
write(str_w,'(a,i3)') ". number of project zones      :",prj%nzone
call print_info(8,adjustl(str_w))

! ----------------------------------------------------------------------------
! Read number of coupling relations between zones (0 by default)

call rpmgetkeyvalint(pcour, "NCOUPLING", prj%ncoupling, 0)
write(str_w,'(a,i3)') ". number of coupling relations :",prj%ncoupling
call print_info(8,adjustl(str_w))

! ----------------------------------------------------------------------------
! Framework type (needed)

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
  call erreur("parameter parsing","type de repere inconnu")
endselect

! ----------------------------------------------------------------------------
! Read time integration process (needed)

call rpmgetkeyvalstr(pcour, "TIME", str)

prj%typ_temps = ' '
if (samestring(str, "STEADY" ))   prj%typ_temps = stationnaire
if (samestring(str, "UNSTEADY" )) prj%typ_temps = instationnaire
if (samestring(str, "PERIODIC"))  prj%typ_temps = periodique

select case(prj%typ_temps)

case(stationnaire) ! Evolution pseudo-instationnaire
  call print_info(10,"calcul stationnaire (convergence pseudo-instationnaire)")
  if (.not.(rpm_existkey(pcour,"RESIDUALS").or.rpm_existkey(pcour,"NCYCLE"))) then
    call erreur("parameter parsing","parametre RESIDUALS ou NCYCLE manquant")
  endif
  call rpmgetkeyvalreal(pcour, "RESIDUALS", prj%residumax)
  call rpmgetkeyvalint (pcour, "NCYCLE",    prj%ncycle, huge(prj%ncycle))
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
  call erreur("parameter parsing","type d'integration temporelle inconnu")
endselect

! ----------------------------------------------------------------------------
! Read time integration process (default: ACTION = COMPUTE)

call rpmgetkeyvalstr(pcour, "ACTION", str, "COMPUTE")

prj%action = inull
if (samestring(str, "COMPUTE" ))   prj%action = act_compute
if (samestring(str, "ANALYSE" ))   prj%action = act_analyse

select case(prj%action)
case(act_compute)
case(act_analyse)
case default
  call erreur("parameters parsing","unknown action")
endselect



endsubroutine def_project

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! juin 2003 : coupling and exchanges definition
! sept 2003 : parameters for steady computation
! may  2005 : get main action parameter (COMPUTE/ANALYSE)
!------------------------------------------------------------------------------!



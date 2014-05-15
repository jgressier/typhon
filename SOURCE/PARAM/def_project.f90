!------------------------------------------------------------------------------!
! Procedure : def_project 
!
! Fonction
!   Parse main file parameters / main parameters for the project
!
!------------------------------------------------------------------------------!
subroutine def_project(block, prj)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_GEN
use GEO3D

implicit none

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_project) :: prj

! -- Internal Variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey, nkey2    ! nombre de clefs
integer                  :: i, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

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

prj%typ_coord = inull
if (samestring(str, "2D" ))     prj%typ_coord = geo_2d
if (samestring(str, "2DPLAN" )) prj%typ_coord = geo_2d
if (samestring(str, "2DAXI"))   prj%typ_coord = geo_2daxi
if (samestring(str, "3D"))      prj%typ_coord = geo_3d

select case(prj%typ_coord)
case(geo_2d)
  call print_info(10,"2D framework")
case(geo_2daxi) 
  call print_info(10,"Axisymmetric 2D framework")
  ! DEV : lecture de la position de l'axe
case(geo_3d) 
  call print_info(10,"3D framework")
case default
  call erreur("parameter parsing","unknown type of framework (def_project)")
endselect

! ----------------------------------------------------------------------------
! Read time integration process (needed)

call rpmgetkeyvalstr(pcour, "TIME", str)

prj%time_model = ' '
if (samestring(str, "STEADY" ))   prj%time_model = time_steady
if (samestring(str, "UNSTEADY" )) prj%time_model = time_unsteady
if (samestring(str, "PERIODIC"))  prj%time_model = time_unsteady_periodic
if (samestring(str, "INVERSE"))   prj%time_model = time_unsteady_inverse

select case(prj%time_model)

case(time_steady) ! Evolution pseudo-instationnaire

  call print_info(10,"STEADY computation (pseudo unsteady convergence)")
  if (.not.(rpm_existkey(pcour,"RESIDUALS").or.rpm_existkey(pcour,"NCYCLE"))) then
    call erreur("parameter parsing","RESIDUALS or NCYCLE parameter not found (def_project)")
  endif
  call rpmgetkeyvalreal(pcour, "RESIDUALS", prj%residumax)
  call rpmgetkeyvalint (pcour, "NCYCLE",    prj%ncycle, huge(prj%ncycle))
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS
  
case(time_unsteady) ! Evolution instationnaire

  call print_info(10,"UNSTEADY computation")
  call rpmgetkeyvalreal(pcour, "DURATION", prj%duration)
  if (rpm_existkey(pcour,"BASETIME").and.rpm_existkey(pcour,"NCYCLE")) then
    call erreur("parameter parsing","BASETIME and NCYCLE parameters conflict (def_project)")
  endif
  if (rpm_existkey(pcour,"BASETIME")) then
    call print_info(10,"!!! Warning !!! it not advised to use BASETIME parameter...")
    call rpmgetkeyvalreal(pcour, "BASETIME", prj%dtbase)
    prj%ncycle = int((prj%duration+2.*tiny(prj%dtbase))/prj%dtbase)
    prj%dtbase = prj%duration / prj%ncycle
  elseif (rpm_existkey(pcour,"NCYCLE")) then
    call rpmgetkeyvalint(pcour, "NCYCLE", prj%ncycle)
    prj%dtbase = prj%duration / prj%ncycle
  else
    call erreur("parameter parsing","NCYCLE (or BASETIME) parameter not found (def_project)")
  endif
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS

case(time_unsteady_inverse) ! Evolution unsteady + inverse method

  call print_info(10,"UNSTEADY computation with INVERSE computation")
  call rpmgetkeyvalreal(pcour, "DURATION", prj%duration)
  call rpmgetkeyvalint(pcour, "NCYCLE",    prj%ncycle)
  prj%dtbase = prj%duration / prj%ncycle

  call def_inverse(prj, block, prj%inverse)
  prj%ncycle = prj%ncycle - prj%inverse%ncyc_futur

  ! DEV : TRAITER LES MOTS CLEFS INTERDITS

case(time_unsteady_periodic) ! Evolution periodique

  call print_info(10,"UNSTEADY AND PERIODICAL computation")
  call rpmgetkeyvalreal(pcour, "PERIOD", prj%duration)
  call rpmgetkeyvalint (pcour, "NCYCLE", prj%ncycle)
  prj%dtbase = prj%duration / prj%ncycle
  ! DEV : TRAITER LES MOTS CLEFS INTERDITS
  call erreur("Developpement","calcul periodique non implemente")

case default
  call erreur("parameter parsing","unknown TIME model")
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



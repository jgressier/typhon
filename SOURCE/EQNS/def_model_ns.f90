!------------------------------------------------------------------------------!
! Procedure : def_model_ns                Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cd historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres de définition du modèle de conduction de la chaleur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_model_ns(block, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NS

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_solver)       :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition du modèle fluide")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnuméraire")

defsolver%nequat = 5

! -- lecture du type de modélisation

call rpmgetkeyvalstr(pcour, "DYNAMICS", str)

if (samestring(str, "EULER"))    defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "PERFECT"))  defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "LAMINAR"))  defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "RANS"))     defsolver%defns%typ_fluid = eqRANS

select case(defsolver%defns%typ_fluid)
case(eqEULER)
  call print_info(10,"    équations de fluide parfait (Euler)")

case(eqNSLAM)
  call print_info(10,"    équations de Navier-Stokes laminaires")
  call erreur("Développement", "Pas d'implémentation des termes visqueux")

case(eqRANS)
  call print_info(10,"    équations de Navier-Stokes moyennées (RANS)")
  call erreur("Développement", "Pas d'implémentation de la turbulence")

case default
  call erreur("lecture de menu", "modélisation de la dynamique inconnue (DYNAMICS)")
endselect


endsubroutine def_model_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! sept 2003 : création de la procédure
!------------------------------------------------------------------------------!

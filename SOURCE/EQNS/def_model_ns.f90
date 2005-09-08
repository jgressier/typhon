!------------------------------------------------------------------------------!
! Procedure : def_model_ns                Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definition du modele de conduction de la chaleur
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

! -- Declaration des entrees --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_solver)       :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition du modele fluide")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnumeraire")

defsolver%nequat = 5

! -- lecture du type de modelisation

call rpmgetkeyvalstr(pcour, "DYNAMICS", str)

if (samestring(str, "EULER"))    defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "PERFECT"))  defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "LAMINAR"))  defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "RANS"))     defsolver%defns%typ_fluid = eqRANS

select case(defsolver%defns%typ_fluid)
case(eqEULER)
  call print_info(10,"    equations de fluide parfait (Euler)")
  
  ! no viscosity
  defsolver%defns%typ_visc = visc_no

case(eqNSLAM)
  call print_info(10,"    equations de Navier-Stokes laminaires")

  ! -- Reading of viscosity properties
  call rpmgetkeyvalstr(pcour, "VISCOSITY", str,"SUTHERLAND")

  if (samestring(str, "SUTHERLAND"))    defsolver%defns%typ_visc = visc_suth
  if (samestring(str, "CONSTANT"))      defsolver%defns%typ_visc = visc_cst
  if (samestring(str, "LINEAR"))        defsolver%defns%typ_visc = visc_lin
  
case(eqRANS)
  call print_info(10,"    equations de Navier-Stokes moyennees (RANS)")
  call erreur("Developpement", "Pas d'implementation de la turbulence")

case default
  call erreur("lecture de menu", "modelisation de la dynamique inconnue (DYNAMICS)")
endselect

! -- lecture des proprietes du gaz

call rpmgetkeyvalstr(pcour, "GAS", str, "AIR")

if (samestring(str, "AIR"))    defsolver%defns%typ_gas = gas_AIR

select case(defsolver%defns%typ_gas)
case(gas_AIR)
  call print_info(10,"    gaz ideal 'AIR'")
  defsolver%defns%nb_species = 1
  allocate(defsolver%defns%properties(defsolver%defns%nb_species))
  ! -- definition d'espece 1
  defsolver%defns%properties(1)%gamma    =   1.40_krp
  defsolver%defns%properties(1)%r_const  = 287.14_krp
  defsolver%defns%properties(1)%prandtl  =   0.72_krp
  select case(defsolver%defns%typ_visc)
  case(visc_suth)
    defsolver%defns%properties(1)%visc_dyn =   0.0000168_krp
    defsolver%defns%properties(1)%tref     =   273.00_krp
    defsolver%defns%properties(1)%tsuth    =   110.50_krp
  case(visc_cst)
    call rpmgetkeyvalreal(pcour, "DYN_VISC",  defsolver%defns%properties(1)%visc_dyn)
  case(visc_lin)
    call rpmgetkeyvalreal(pcour, "COEF",  defsolver%defns%properties(1)%visc_dyn)
  case(visc_no)
    defsolver%defns%properties(1)%visc_dyn =   0.00_krp
  case default
    call erreur("viscosity computation", "unknown kind of computation")
  endselect
case default
  call erreur("lecture de menu", "modelisation du gas inconnue (GAS)")
endselect


endsubroutine def_model_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! sept 2003 : creation de la procedure
! nov  2003 : definition du fluide (Euler) et du gaz (AIR mono espece)
!------------------------------------------------------------------------------!

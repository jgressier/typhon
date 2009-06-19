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

! -- INPUTS --
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_solver)       :: defsolver

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
real(krp)                :: molweight

! -- BODY --

call print_info(5,"- Definition of Fluid Model")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnumeraire")

defsolver%defns%nb_species = 1
allocate(defsolver%defns%properties(defsolver%defns%nb_species))

! -- Model --

call rpmgetkeyvalstr(pcour, "DYNAMICS", str)

if (samestring(str, "INVISCID")) defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "EULER"))    defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "PERFECT"))  defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "LAMINAR"))  defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "DNS"))      defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "RANS"))     defsolver%defns%typ_fluid = eqRANS

defsolver%nequat = 5
defsolver%nsca   = 2
defsolver%nvec   = 1
allocate(defsolver%idsca(defsolver%nsca))
allocate(defsolver%idvec(defsolver%nvec))
defsolver%idsca(1) = qs_density   ! 'Density'
defsolver%idsca(2) = qs_pressure  ! 'Pressure'
defsolver%idvec(1) = qv_velocity  ! 'Velocity'

select case(defsolver%defns%typ_fluid)
case(eqEULER)
  call print_info(10,"    Inviscid (Euler) equations")
  
  ! no viscosity
  defsolver%defns%properties(1)%typ_visc = visc_no

case(eqNSLAM)
  call print_info(10,"    Laminar Navier-Stokes equations")

  ! -- Reading of viscosity properties
  call rpmgetkeyvalstr(pcour, "VISCOSITY", str, "SUTHERLAND")

  if (samestring(str, "SUTHERLAND"))         defsolver%defns%properties(1)%typ_visc = visc_suth
  if (samestring(str, "CONSTANT"))           defsolver%defns%properties(1)%typ_visc = visc_dyncst ! legacy
  if (samestring(str, "DYNAMIC-CONSTANT"))   defsolver%defns%properties(1)%typ_visc = visc_dyncst
  if (samestring(str, "KINEMATIC-CONSTANT")) defsolver%defns%properties(1)%typ_visc = visc_kincst
  if (samestring(str, "LINEAR"))             defsolver%defns%properties(1)%typ_visc = visc_lin
  
case(eqRANS)
  call print_info(10,"    Reynolds Averaged Navier-Stokes (RANS) equations")
  call erreur("Development", "Pas d'implementation de la turbulence")

case default
  call erreur("Parameter Parsing", "modelisation de la dynamique inconnue (DYNAMICS)")
endselect


! -- GAS properties --

call rpmgetkeyvalstr(pcour, "GAS", str, "AIR")

if (samestring(str, "AIR"))        defsolver%defns%typ_gas = gas_AIR
if (samestring(str, "PERFECT"))    defsolver%defns%typ_gas = gas_PERFECT

select case(defsolver%defns%typ_gas)

case(gas_AIR)
  call print_info(10,"    Perfect Gas 'AIR'")
  ! -- definition d'espece 1
  defsolver%defns%properties(1)%gamma    =   1.40_krp
  defsolver%defns%properties(1)%r_const  = 287.14_krp
  call rpmgetkeyvalreal(pcour, "PRANDTL", defsolver%defns%properties(1)%prandtl, 0.72_krp)

case(gas_PERFECT)
  call print_info(10,"    Perfect Gas 'User Defined'")
  ! -- definition d'espece 1
  call rpmgetkeyvalreal(pcour, "GAMMA",     defsolver%defns%properties(1)%gamma)
  call rpmgetkeyvalreal(pcour, "MOLWEIGHT", molweight)
  defsolver%defns%properties(1)%r_const  = perfect_gas_cst / molweight
  call rpmgetkeyvalreal(pcour, "PRANDTL",   defsolver%defns%properties(1)%prandtl)

case default
  call erreur("Parameter Parsing", "unknown gas model (GAS)")
endselect


select case(defsolver%defns%properties(1)%typ_visc)
case(visc_suth)
  select case(defsolver%defns%typ_gas)
  case(gas_AIR)
    defsolver%defns%properties(1)%visc_dyn =   0.0000168_krp
    defsolver%defns%properties(1)%tref     =   273.00_krp
    defsolver%defns%properties(1)%tsuth    =   110.50_krp
  case(gas_PERFECT)
  case default
    call erreur("Parameter Parsing", "unknown gas model (GAS)")
  endselect
case(visc_dyncst)
  call rpmgetkeyvalreal(pcour, "DYN_VISC",  defsolver%defns%properties(1)%visc_dyn)
case(visc_kincst)
  call rpmgetkeyvalreal(pcour, "KIN_VISC",  defsolver%defns%properties(1)%visc_kin)
case(visc_lin)
  call rpmgetkeyvalreal(pcour, "COEF",  defsolver%defns%properties(1)%visc_dyn)
case(visc_no)
  defsolver%defns%properties(1)%visc_dyn =   0.00_krp
case default
  call erreur("viscosity computation", "unknown kind of computation")
endselect


endsubroutine def_model_ns

!------------------------------------------------------------------------------!
! Changes history
!
! sept 2003 : creation de la procedure
! nov  2003 : definition du fluide (Euler) et du gaz (AIR mono espece)
!------------------------------------------------------------------------------!

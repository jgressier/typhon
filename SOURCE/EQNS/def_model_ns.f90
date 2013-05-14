!------------------------------------------------------------------------------!
! Procedure : def_model_ns                Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definition du modele de conduction de la chaleur
!
!------------------------------------------------------------------------------!
subroutine def_model_ns(prj, block, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_GEN
use MENU_NS
use FCT_PARSER

implicit none

! -- INPUTS --
type(mnu_project)      :: prj
type(rpmblock), target :: block

! -- OUTPUTS --
type(mnu_solver)       :: defsolver

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
real(krp)                :: molweight

! -- BODY --

call print_info(5,"- Definition of Fluid Model")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call error_stop("too many or missing MODEL block")

defsolver%defns%nb_species = 1
allocate(defsolver%defns%properties(defsolver%defns%nb_species))

! -- Model --

call rpmgetkeyvalstr(pcour, "DYNAMICS", str)

if (samestring(str, "INVISCID"))     defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "INVISCID-AXI")) defsolver%defns%typ_fluid = eqEULERaxi
if (samestring(str, "EULER"))        defsolver%defns%typ_fluid = eqEULER
if (samestring(str, "EULER-AXI"))    defsolver%defns%typ_fluid = eqEULERaxi
if (samestring(str, "LAMINAR"))      defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "LAMINAR-AXI"))  defsolver%defns%typ_fluid = eqNSLAMaxi
if (samestring(str, "DNS"))          defsolver%defns%typ_fluid = eqNSLAM
if (samestring(str, "DNS-AXI"))      defsolver%defns%typ_fluid = eqNSLAMaxi
if (samestring(str, "RANS"))         defsolver%defns%typ_fluid = eqRANS

select case(prj%typ_coord)
case(geo_2d)
  select case(defsolver%defns%typ_fluid)
  case(eqEULER, eqNSLAM, eqEULERaxi, eqNSLAMaxi) ! nothing to do
  case(eqNSLES)
    call error_stop("NS LES model is not available on 2D mesh")
  case default
    call error_stop("unknown parameter for DYNAMICS")
  endselect
case(geo_2daxi)
  select case(defsolver%defns%typ_fluid)
  case(eqEULERaxi, eqNSLAMaxi) ! nothing to do
  case(eqEULER)
    defsolver%defns%typ_fluid = eqEULERaxi
  case(eqNSLAM)
    defsolver%defns%typ_fluid = eqNSLAMaxi
  case(eqNSLES)
    call error_stop("NS LES model is not available on 2D mesh")
  case default
    call error_stop("unknown parameter for DYNAMICS")
  endselect
case(geo_3d)
  select case(defsolver%defns%typ_fluid)
  case(eqEULER, eqNSLAM, eqEULERaxi, eqNSLAMaxi, eqNSLES) ! nothing to do
  case default
    call error_stop("unknown parameter for DYNAMICS")
  endselect
case default
  call error_stop("unknown parameter for project COORD (NS model routine)")
endselect

call define_solver(defsolver, 2, 1)

defsolver%idsca(1) = qs_density   ! 'Density'
defsolver%idsca(2) = qs_pressure  ! 'Pressure'
defsolver%idvec(1) = qv_velocity  ! 'Velocity'

select case(defsolver%defns%typ_fluid)
case(eqEULER)
  call print_info(10,"    Inviscid (Euler) equations")  
case(eqEULERaxi)
  call print_info(10,"    Inviscid (Euler) AXISYMMETRIC equations")  
case(eqNSLAM)
  call print_info(10,"    Laminar Navier-Stokes equations")
case(eqNSLAMaxi)
  call print_info(10,"    Laminar AXISYMMETRIC Navier-Stokes equations")
case(eqRANS)
  call print_info(10,"    Reynolds Averaged Navier-Stokes (RANS) equations")
  call error_stop("Development: RANS turbulence models have not been implemented")
case default
  call error_stop("Parameter Parsing: unknown parameter for DYNAMICS")
endselect

select case(defsolver%defns%typ_fluid)
case(eqEULER, eqEULERaxi)
  ! no viscosity
  defsolver%defns%properties(1)%typ_visc = visc_no
case(eqNSLAM, eqNSLAMaxi)
  ! -- Reading of viscosity properties
  call rpmgetkeyvalstr(pcour, "VISCOSITY", str, "SUTHERLAND")
  if (samestring(str, "SUTHERLAND"))         defsolver%defns%properties(1)%typ_visc = visc_suth
  if (samestring(str, "CONSTANT"))           defsolver%defns%properties(1)%typ_visc = visc_dyncst ! legacy
  if (samestring(str, "DYNAMIC-CONSTANT"))   defsolver%defns%properties(1)%typ_visc = visc_dyncst
  if (samestring(str, "KINEMATIC-CONSTANT")) defsolver%defns%properties(1)%typ_visc = visc_kincst
  if (samestring(str, "LINEAR"))             defsolver%defns%properties(1)%typ_visc = visc_lin
  call print_info(10,"    . viscosity model is "//trim(str))
case(eqRANS)
  call error_stop("Development: RANS turbulence models have not been implemented")
case default
  call error_stop("unknown DYNAMICS parameter")
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
! NIST reference for molar gas constant : R = 8.3144621(75) J/mol/K
! ref: http://physics.nist.gov/cgi-bin/cuu/Value?r
! US standard atmosphere : mean molecular weight of air : M = 0.0289644 kg/mol
! ref: https://docs.google.com/open?id=0B2UKsBO-ZMVgWG9mWEJGMlFacDQ, p. 9 (25)
! ??  defsolver%defns%properties(1)%r_const  = 287.057978_krp
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
  call error_stop("Parameter Parsing: unknown gas model (GAS)")
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
    call error_stop("unknown gas model (GAS)")
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
  call error_stop("viscosity computation: unknown kind of computation")
endselect

! -- External forces and energy source --

defsolver%defns%is_extforce = rpm_existkey(pcour, "EXT_FORCE_X").or. &
                              rpm_existkey(pcour, "EXT_FORCE_Y").or. &
                              rpm_existkey(pcour, "EXT_FORCE_Z")
if (defsolver%defns%is_extforce) then
  call print_info(10,"    . additional external force")
  call rpmgetkeyvalstr(pcour, "EXT_FORCE_X", str, "0.")
  call convert_to_funct(str, defsolver%defns%extforce_x, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pcour, "EXT_FORCE_Y", str, "0.")
  call convert_to_funct(str, defsolver%defns%extforce_y, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
  call rpmgetkeyvalstr(pcour, "EXT_FORCE_Z", str, "0.")
  call convert_to_funct(str, defsolver%defns%extforce_z, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
endif

defsolver%defns%is_extpower = rpm_existkey(pcour, "EXT_POWER")
if (defsolver%defns%is_extpower) then
  call print_info(10,"    . additional external energy source")
  call rpmgetkeyvalstr(pcour, "EXT_POWER", str, "0.")
  call convert_to_funct(str, defsolver%defns%extpower, info)
  if (info /= 0) call error_stop("problem when parsing "//trim(str)) 
endif

endsubroutine def_model_ns
!------------------------------------------------------------------------------!
! Changes history
!
! sept 2003: creation de la procedure
! nov  2003: definition du fluide (Euler) et du gaz (AIR mono espece)
! sep  2005: various viscosity laws (constant, linear)
! may  2009: various viscosity laws (constant kinematic)
! feb  2011: external sources (forces and energy)
! Apr  2011: axisymmetric solver
!------------------------------------------------------------------------------!

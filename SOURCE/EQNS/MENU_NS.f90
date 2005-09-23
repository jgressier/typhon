!------------------------------------------------------------------------------!
! MODULE : MENU_NS                        Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : (cf historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options des solveurs EULER, NS, RANS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_NS

use TYPHMAKE   ! Definition de la precision
use VARCOM     ! Definition des parametres constantes
use EQNS       ! Definition des proprietes gaz

implicit none

! -- Variables globales du module -------------------------------------------

! -- Type de solveur (menu_ns%typ_fluid)--
integer, parameter :: eqEULER = 10
integer, parameter :: eqNSLAM = 11 
integer, parameter :: eqRANS  = 12

! -- Type de gaz (menu_ns%typ_gaz) --
integer, parameter :: gas_AIR = 10

! -- Viscosity : computation
integer, parameter :: visc_no = 10
integer, parameter :: visc_suth = 11
integer, parameter :: visc_cst =12
integer, parameter :: visc_lin = 13


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_NS : options numeriques les solveurs Euler, NS et RANS
!------------------------------------------------------------------------------!
type mnu_ns
  integer         :: typ_fluid         ! type de fluide (cf definitions parameter) 
  integer         :: typ_gas           ! type de gaz    (cf definitions parameter)
  integer         :: typ_visc          ! kind of computation of viscosity
  integer         :: nb_species        ! nombre d'especes resolues
  type(st_espece), dimension(:), pointer &
                  :: properties        ! proprietes des differentes especes
endtype mnu_ns

!------------------------------------------------------------------------------!
! structure ST_BOCO_NS : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_boco_ns
  ! definir un etat
  real(krp) :: pstat, ptot, ttot, mach
  real(krp) :: temp_wall
  type(v3d) :: direction
  real(krp), dimension(:), pointer  &
            :: temp           ! not uniform wall temperature
  logical   :: alloctemp      ! allocation of table "temp"
  logical   :: allocflux      ! allocation of table "flux_nunif"
  logical   :: allochconv     ! allocation of tables "h_nunif" and "tconv_nunif"
  character (len=strlen) &
            :: tempfile       ! file name for definition of not uniform temperature  
  real(krp) :: flux
  real(krp), dimension(:), pointer  &
            :: flux_nunif     ! not uniform wall heat flux
  character (len=strlen) &
            :: fluxfile       ! file name for definition of not uniform flux
  real(krp), dimension(:), pointer &
            :: h_nunif        ! not uniform convection coefficient (for coupling)
  real(krp), dimension(:), pointer &
            :: tconv_nunif    ! not uniform convection reference temperature (coupling) 
endtype st_boco_ns

!------------------------------------------------------------------------------!
! structure ST_INIT_NS : Definition de l'initialisation
!------------------------------------------------------------------------------!
type st_init_ns
  ! definir un etat
  real(krp) :: pstat, ptot, ttot, mach
  type(v3d) :: direction
endtype st_init_ns


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! fonction : retourne le type de calcul selon le type physique de cond. lim.
!------------------------------------------------------------------------------!
integer function bctype_of_nsboco(bocotype)
implicit none
integer bocotype

  select case(bocotype)
  case(bc_wall_adiab)
    bctype_of_nsboco = bc_calc_ghostcell
  case(bc_wall_isoth)
    bctype_of_nsboco = bc_calc_ghostcell
  case(bc_wall_flux)
    bctype_of_nsboco = bc_calc_ghostcell
  case(bc_inlet_sup)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_inlet_sub)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_outlet_sup)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_outlet_sub)
    bctype_of_nsboco = bc_calc_ghostface
  case default
    call erreur("incoherence interne (MENU_NS)",&
                "type de conditions aux limites inattendu")
  endselect

endfunction bctype_of_nsboco


endmodule MENU_NS
!------------------------------------------------------------------------------!
! Historique des modifications
!
! aout 2002 : creation du module
! juin 2004 : conditions limites (bctype_of_nsboco, st_boco_ns) 
!------------------------------------------------------------------------------!





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
use FCT_NODE

implicit none



! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_NS : options numeriques les solveurs Euler, NS et RANS
!------------------------------------------------------------------------------!
type mnu_ns
  integer           :: typ_fluid         ! type de fluide (cf definitions parameter) 
  integer           :: typ_gas           ! type de gaz    (cf definitions parameter)
  integer           :: nb_species        ! nombre d'especes resolues
  type(st_espece), dimension(:), pointer &
                    :: properties        ! proprietes des differentes especes
  logical           :: is_extforce, is_extpower, xyz_depend
  type(st_fct_node) :: extforce_x, extforce_y, extforce_z
  type(st_fct_node) :: extpower
endtype mnu_ns

!------------------------------------------------------------------------------!
! structure ST_BOCO_NS : Boundary conditions parameters
!------------------------------------------------------------------------------!
type st_boco_ns
  logical   :: is_ttot   ! else entropy
  type(st_fct_node) :: pstat, ptot, ttot, mach, entropy       ! FCT functions
  real(krp) :: temp_wall
  type(v3d) :: wall_velocity
  type(st_fct_node) :: dir_x, dir_y, dir_z
  logical   :: xyz_depend
  real(krp), dimension(:), pointer  &
            :: temp           ! not uniform wall temperature
  logical   :: alloctemp      ! allocation of table "temp"
  logical   :: allocflux      ! allocation of table "flux_nunif"
  logical   :: allochconv     ! allocation of tables "h_nunif" and "tconv_nunif"
  character (len=longname) &
            :: tempfile       ! file name for definition of not uniform temperature  
  real(krp) :: flux
  real(krp), dimension(:), pointer  &
            :: flux_nunif     ! not uniform wall heat flux
  character (len=longname) &
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
  ! multiple ways to define a state
  logical :: is_density, is_pstat, is_tstat, is_velocity, is_vcomponent
  type(st_fct_node) :: density, pstat, ptot, tstat, ttot, mach, velocity    ! FCT functions
  type(st_fct_node) :: dir_x, dir_y, dir_z, vx, vy, vz                      ! FCT functions
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
! Change history
!
! aout 2002 : creation du module
! june 2004 : conditions limites (bctype_of_nsboco, st_boco_ns) 
! sept 2006 : FCT functions for initialization variables
! febr 2011 : v3d direction became st_fct_node dir_x,dir_y,dir_z (A.Gardi)
!------------------------------------------------------------------------------!





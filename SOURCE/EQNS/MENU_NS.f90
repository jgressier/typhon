!------------------------------------------------------------------------------!
! MODULE : MENU_NS                        Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : (cf historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options des solveurs EULER, NS, RANS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_NS

use TYPHMAKE   ! Definition de la precision
use VARCOM     ! Définition des paramètres constantes
use EQNS       ! Définition des propriétés gaz

implicit none

! -- Variables globales du module -------------------------------------------

! -- Type de solveur (menu_ns%typ_fluid)--
integer, parameter :: eqEULER = 10
integer, parameter :: eqNSLAM = 11 
integer, parameter :: eqRANS  = 12

! -- Type de gaz (menu_ns%typ_gaz) --
integer, parameter :: gas_AIR = 10


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_NS : options numériques les solveurs Euler, NS et RANS
!------------------------------------------------------------------------------!
type mnu_ns
  integer         :: typ_fluid         ! type de fluide (cf definitions parameter) 
  integer         :: typ_gas           ! type de gaz    (cf definitions parameter) 
  integer         :: nb_species        ! nombre d'espèces résolues
  type(st_espece), dimension(:), pointer &
                  :: properties        ! propriétés des différentes espèces
endtype mnu_ns

!------------------------------------------------------------------------------!
! structure ST_BOCO_NS : Définition des conditions aux limites
!------------------------------------------------------------------------------!
type st_boco_ns
  ! définir un état
  real(krp) :: pstat, ptot, ttot, mach
  real(krp) :: temp_wall
  type(v3d) :: direction
endtype st_boco_ns

!------------------------------------------------------------------------------!
! structure ST_INIT_NS : Définition de l'initialisation
!------------------------------------------------------------------------------!
type st_init_ns
  ! définir un état
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
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_wall_isoth)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_wall_flux)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_inlet_sup)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_inlet_sub)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_outlet_sup)
    bctype_of_nsboco = bc_calc_ghostface
  case(bc_outlet_sub)
    bctype_of_nsboco = bc_calc_ghostface
  case default
    call erreur("incohérence interne (MENU_NS)",&
                "type de conditions aux limites inattendu")
  endselect

endfunction bctype_of_nsboco


endmodule MENU_NS
!------------------------------------------------------------------------------!
! Historique des modifications
!
! aout 2002 : création du module
! juin 2004 : conditions limites (bctype_of_nsboco, st_boco_ns) 
!------------------------------------------------------------------------------!





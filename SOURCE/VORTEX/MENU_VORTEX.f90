!------------------------------------------------------------------------------!
! MODULE : MENU_VORTEX                    Auteur : J. Gressier
!                                         Date   : Février 2004
! Fonction                                Modif  : (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options du solveur VORTEX
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_VORTEX

use TYPHMAKE   ! Definition de la precision
use VARCOM
use MGRID      

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

! constantes : type de modélisation des vortex libres
character, parameter :: vort_sng  = 'S'
character, parameter :: vort_blob = 'B'
character, parameter :: vort_ring = 'R'
character, parameter :: vort_fil  = 'F'
character, parameter :: blob_visq = 'V'

! constantes : type de calcul de la condition KUTTA-JOUKOWSKI
character, parameter :: kt_equilibrium = 'E'
character, parameter :: kt_shedding    = 'V'

!------------------------------------------------------------------------------!
! structure MNU_VORTEX : options numériques du solveur de diffusion (thermique)
!------------------------------------------------------------------------------!
type mnu_vort
  character :: typ_vortex
  character :: typ_blob
  real(krp) :: radius
endtype mnu_vort


!------------------------------------------------------------------------------!
! structure ST_BOCO_VORT : Définition des conditions aux limites
!------------------------------------------------------------------------------!
type st_boco_vort
  integer                 :: element    ! type d'élément si répartition de singularités
  character               :: mode       ! mode de traitement 
  type(v3d)               :: vect       ! définition d'un vecteur
  type(st_grid), pointer  :: pgrid      ! pointeur sur une grille
endtype st_boco_vort


!------------------------------------------------------------------------------!
! structure ST_INIT_VORT : Définition des conditions initiales
!------------------------------------------------------------------------------!
type st_init_vort
  real(krp) :: pipo
endtype st_init_vort



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------

!integer bctype_of_vortboco

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! fonction : retourne le type de calcul selon le type physique de cond. lim.
!------------------------------------------------------------------------------!
integer function bctype_of_vortboco(bocotype)
implicit none
integer bocotype

  select case(bocotype)
  case(bc_wall)
    bctype_of_vortboco = bc_calc_singpanel
  case(bc_kutta)
    bctype_of_vortboco = bc_calc_kutta
  case(bc_farfield)
    bctype_of_vortboco = bc_calc_farfield
  case default
    call erreur("incohérence interne (MENU_VORTEX)",&
                "type de conditions aux limites inattendu pour le solveur VORTEX")
  endselect

endfunction bctype_of_vortboco


endmodule MENU_VORTEX

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : création du module
!------------------------------------------------------------------------------!

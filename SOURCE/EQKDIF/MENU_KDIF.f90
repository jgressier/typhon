!------------------------------------------------------------------------------!
! MODULE : MENU_KDIF                      Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options du solveur KDIF
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_KDIF

use TYPHMAKE   ! Definition de la precision
use VARCOM
use MATERIAU  ! Définition du matériau
!use EQKDIF    ! Définition des propriétés températures et matériau

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_KDIF : options numériques du solveur de diffusion (thermique)
!------------------------------------------------------------------------------!
type mnu_kdif
  type(st_materiau)   :: materiau      ! type de matériau
endtype mnu_kdif


!------------------------------------------------------------------------------!
! structure ST_BOCO_KDIF : Définition des conditions aux limites
!------------------------------------------------------------------------------!
type st_boco_kdif
  real(krp) :: temp_ext       ! température extérieure pour le rayonnement
  real(krp) :: temp_wall      ! température de paroi (si isotherme)
  real(krp), dimension(:), pointer  &
            :: temp           ! température de paroi non uniforme
  logical   :: alloctemp      ! allocation du tableau temp
  character (len=strlen) &
            :: tempfile       ! nom de fichier de définition de la temp non uniforme  
  real(krp), dimension(:), pointer  &
            :: flux_nunif     ! flux non uniforme
  logical   :: allocflux      ! allocation du tableau flux_nunif
  character (len=strlen) &
            :: fluxfile       ! nom de fichier de définition du flux non uniforme  
  real(krp) :: h_conv         ! coefficient de convection
  real(krp) :: temp_conv      ! température de relaxation pour la convection
  real(krp) :: flux           ! flux (si flux imposé)
endtype st_boco_kdif


!------------------------------------------------------------------------------!
! structure ST_INIT_KDIF : Définition des conditions aux limites
!------------------------------------------------------------------------------!
type st_init_kdif
  real(krp) :: temp           ! température du champ
endtype st_init_kdif



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------

!integer bctype_of_kdifboco

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! fonction : retourne le type de calcul selon le type physique de cond. lim.
!------------------------------------------------------------------------------!
integer function bctype_of_kdifboco(bocotype)
implicit none
integer bocotype

  select case(bocotype)
  case(bc_wall_adiab)
    bctype_of_kdifboco = bc_calc_flux
  case(bc_wall_isoth)
    bctype_of_kdifboco = bc_calc_ghostface
  case(bc_wall_flux)
    bctype_of_kdifboco = bc_calc_flux
  case(bc_wall_hconv)
    bctype_of_kdifboco = bc_calc_ghostface
  case default
    call erreur("incohérence interne (MENU_KDIF)",&
                "type de conditions aux limites inattendu")
  endselect

endfunction bctype_of_kdifboco


endmodule MENU_KDIF

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 (v0.0.1b): création du module
! nov 2003           : ajout de la température non uniforme de paroi 
!                      (CL Dirichlet)
!------------------------------------------------------------------------------!

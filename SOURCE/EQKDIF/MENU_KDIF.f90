!------------------------------------------------------------------------------!
! MODULE : MENU_KDIF                      Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options du solveur KDIF
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_KDIF

use TYPHMAKE   ! Definition de la precision
use VARCOM
use MATERIAU  ! Definition du materiau
!use EQKDIF    ! Definition des proprietes temperatures et materiau

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_KDIF : options numeriques du solveur de diffusion (thermique)
!------------------------------------------------------------------------------!
type mnu_kdif
  type(st_materiau)   :: materiau      ! type de materiau
endtype mnu_kdif


!------------------------------------------------------------------------------!
! structure ST_BOCO_KDIF : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_boco_kdif
  real(krp) :: temp_ext       ! temperature exterieure pour le rayonnement
  real(krp) :: temp_wall      ! temperature de paroi (si isotherme)
  real(krp), dimension(:), pointer  &
            :: temp           ! temperature de paroi non uniforme
  logical   :: alloctemp      ! allocation du tableau temp
  character (len=strlen) &
            :: tempfile       ! nom de fichier de definition de la temp non uniforme  
  real(krp) :: flux           ! flux (si flux impose)
  real(krp), dimension(:), pointer  &
            :: flux_nunif     ! flux non uniforme
  logical   :: allocflux      ! allocation du tableau flux_nunif
  character (len=strlen) &
            :: fluxfile       ! nom de fichier de definition du flux non uniforme  
  real(krp) :: h_conv         ! coefficient de convection
  real(krp) :: temp_conv      ! temperature de relaxation pour la convection
  real(krp), dimension(:), pointer  &
            :: h_nunif        ! coefficient de convection non uniforme
  real(krp), dimension(:), pointer  &
            :: tconv_nunif    ! temperature de convection non uniforme
  logical   :: allochconv     ! allocation des tableaux h_nunif et tconv_nunif
  character (len=strlen) &
            :: hfile          ! nom de fichier de definition du coef de convection non uniforme
  character (len=strlen) &
            :: tconvfile       ! nom de fichier de definition de la temperature de 
                               ! convection non uniforme  
endtype st_boco_kdif


!------------------------------------------------------------------------------!
! structure ST_INIT_KDIF : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_init_kdif
  real(krp) :: temp           ! temperature du champ
  real(krp), dimension(:), pointer &
            :: coef           ! coefficients pour la variation spatialement 
                              ! lineaire de la condition initiale, provisoire
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
    bctype_of_kdifboco = bc_calc_ghostface
    !bctype_of_kdifboco = bc_calc_flux
  case(bc_wall_isoth)
    bctype_of_kdifboco = bc_calc_ghostface
  case(bc_wall_flux)
    bctype_of_kdifboco = bc_calc_ghostface
    !bctype_of_kdifboco = bc_calc_flux
  case(bc_wall_hconv)
    bctype_of_kdifboco = bc_calc_ghostface
  case default
    call erreur("incoherence interne (MENU_KDIF)",&
                "type de conditions aux limites inattendu")
  endselect

endfunction bctype_of_kdifboco


endmodule MENU_KDIF

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 (v0.0.1b): creation du module
! nov 2003           : ajout de la temperature non uniforme de paroi 
!                      (CL Dirichlet)
! juin 2004          : conditions de Neumann et Fourier non uniformes
!------------------------------------------------------------------------------!

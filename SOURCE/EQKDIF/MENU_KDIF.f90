!------------------------------------------------------------------------------!
! MODULE : MENU_KDIF                                 Author  : J. Gressier
!                                                    Created : November 2002
! Fonction                                           Modif   : (cf History)
!   Structure definition for parameter input (Diffusion solver)
!
!------------------------------------------------------------------------------!
module MENU_KDIF

use TYPHMAKE     ! Definition de la precision
use VARCOM
use MATERIAU     ! Definition du materiau
use SPARSE_MAT
!use EQKDIF    ! Definition des proprietes temperatures et materiau

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: rad_none    = 0  ! no radiation emmission
integer(kpp), parameter :: rad_direct  = 1  ! simple emmission local T^4 and reception Tinf^4
integer(kpp), parameter :: rad_coupled = 2  ! reception Tinf^4 + integration of all emmitting faces

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_KDIF : options numeriques du solveur de diffusion (thermique)
!------------------------------------------------------------------------------!
type mnu_kdif
  type(st_materiau)   :: materiau      ! type de materiau
  real(krp)           :: tolerance     ! tolerance to ignore view factor
  type(st_sdlu)       :: viewfactor    ! normalized view factor (symmetric sparse matrix shape)
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
  integer(kpp) :: radiating    ! parameter for radiating boundary condition
  real(krp)    :: emmissivity  ! emmissivity of the surface
  real(krp)    :: rad_Tinf     ! far field temperature
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

interface delete
  module procedure delete_mnu_kdif
endinterface

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


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_KDIF
!------------------------------------------------------------------------------!
subroutine delete_mnu_kdif(defkdif)
implicit none
type(mnu_kdif)  :: defkdif

  call delete(defkdif%materiau%Kd)
  call delete(defkdif%viewfactor)

endsubroutine delete_mnu_kdif


endmodule MENU_KDIF

!------------------------------------------------------------------------------!
! Change history
!
! nov  2002 : creation du module
! nov  2003 : ajout de la temperature non uniforme de paroi (CL Dirichlet)
! june 2004 : conditions de Neumann et Fourier non uniformes
! june 2005 : radiating parameters & view factors
!------------------------------------------------------------------------------!

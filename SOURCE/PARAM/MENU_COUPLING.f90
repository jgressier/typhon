!------------------------------------------------------------------------------!
! MODULE : MENU_COUPLING                  Auteur : J. Gressier / E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Définition des méthodes de couplage entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_COUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure COUPLING : definitions des raccords pour une zone
!------------------------------------------------------------------------------!
type mnu_coupling
  integer               :: zone1, zone2  ! numéro des zones concernées par le couplage
  integer               :: typ_calc      ! type de calcul/connection des maillages
  integer               :: period_mode   ! mode de calcul de la périodicité
  integer               :: typ_interpol  ! type de calcul/interpolation
  integer               :: n_tpsbase     ! périodicité du couplage
  real(krp)             :: corcoef      ! coefficient pour correction de flux
endtype mnu_coupling


!------------------------------------------------------------------------------!
! Définition de la structure SENSEUR : senseur pour le déclenchement du couplage
!------------------------------------------------------------------------------!
type mnu_senseur
  integer                      :: mode ! utilisation du senseur
  logical                      :: sens ! "détection" de conditions de déclenchement
  				       ! de couplage
  integer                      :: nmin
  integer                      :: nmax
  real(krp)                    :: ecartflux
  real(krp)                    :: epsilon
endtype mnu_senseur


endmodule MENU_COUPLING

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création du module
! oct 2003           : ajout coeff correction de flux
!------------------------------------------------------------------------------!



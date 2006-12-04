!------------------------------------------------------------------------------!
! MODULE : MENU_COUPLING                  Auteur : J. Gressier / E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Definition des methodes de couplage entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_COUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure COUPLING : definitions des raccords pour une zone
!------------------------------------------------------------------------------!
type mnu_coupling
  integer               :: zone1, zone2  ! numero des zones concernees par le couplage
  integer               :: typ_calc      ! type de calcul/connection des maillages
  integer               :: period_mode   ! mode de calcul de la periodicite
  integer               :: typ_interpol  ! type de calcul/interpolation
  integer               :: n_tpsbase     ! periodicite du couplage
  real(krp)             :: corcoef      ! coefficient pour correction de flux
  integer               :: boco         ! type de condition limite au raccord
endtype mnu_coupling


!------------------------------------------------------------------------------!
! Definition de la structure SENSEUR : senseur pour le declenchement du couplage
!------------------------------------------------------------------------------!
type mnu_senseur
  integer                      :: mode ! utilisation du senseur
  logical                      :: sens ! "detection" de conditions de declenchement
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
! juin 2003 (v0.0.1b): creation du module
! oct 2003           : ajout coeff correction de flux
!------------------------------------------------------------------------------!



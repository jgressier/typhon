!------------------------------------------------------------------------------!
! MODULE : EQNS                           Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : cf historique
!   Bibliotheque de procedures et fonctions pour la définition des états
!   dans les équations de Navier-Stokes
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module EQNS

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Compilation conditionnelle ? avec GEO3D_dp

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_NSETAT : état physique
!------------------------------------------------------------------------------!
type st_nsetat
!  real(krp), dimension(:), pointer &
!                  :: density    ! masses volumiques partielles (nesp)
  real(krp)       :: density    ! masse volumique
  real(krp)       :: pressure   ! pression
  type(v3d)       :: velocity   ! vitesse
endtype st_nsetat

!------------------------------------------------------------------------------!
! Définition de la structure ST_ESPECE : Définition d'une espèce de gaz
!------------------------------------------------------------------------------!
type st_espece
  real(krp)    :: gamma         ! rapport de chaleurs spécifiques
  real(krp)    :: prandtl       ! nombre de Prandtl
  real(krp)    :: visc_dyn      ! viscosité dynamique (faire évoluer en loi)
endtype st_espece

! -- INTERFACES -------------------------------------------------------------

!interface new
!  module procedure new_mesh, new_field, new_block, new_zone
!endinterface

!interface delete
!  module procedure delete_mesh, delete_field, delete_block, delete_zone
!endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains



!------------------------------------------------------------------------------!
! Fonction : conversion de variables conservatives en variables primitives
!------------------------------------------------------------------------------!
!type(st_nsetat) function cons2ns(defns, etat)
!implicit none!
!
! -- déclaration des entrées
!type(mnu_kdif)          :: defns
!real(krp), dimension(*) :: etat

!  cons2kdif%density  = etat(1)
!  cons2kdif%pressure = etat(1)/defkdif%materiau%Cp
!  cons2kdif%density = etat(1)/defkdif%materiau%Cp
!  cons2kdif%density = etat(1)/defkdif%materiau%Cp

!endfunction cons2kdif


endmodule EQNS

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2002 : création du module
! sept 2003 : adaptation du module pour premiers développements
!------------------------------------------------------------------------------!

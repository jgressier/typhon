!------------------------------------------------------------------------------!
! MODULE : MATERIAU                       Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  :
!   Structures pour les paramètres MATERIAU du solveur de thermique
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MATERIAU

use TYPHMAKE      ! définition de la précision des réels
use MATER_LOI    ! définition générale d'une loi de variation des paramètres

implicit none

! -- Variables globales du module -------------------------------------------

character, parameter :: mat_LIN  = 'L'   ! matériau à propriétés constantes
character, parameter :: mat_KNL  = 'K'   ! matériau à cp constant et conductivité non linéaire
character, parameter :: mat_XMAT = 'X'   ! matériau à propriétés spécifiques

!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! ST_MATERIAU : structure pour la définition des équations
!------------------------------------------------------------------------------!
type st_materiau
  character(len=30) :: nom       ! nom du materiau
  character         :: type      ! cf constantes
  real(krp)         :: Cp        ! Capacité calorifique
  type(st_loi)      :: Energie   ! Energie (fct de température)
  type(st_loi)      :: Kd        ! conductivité thermique 
endtype st_materiau


! -- INTERFACES -------------------------------------------------------------


! -- Procédures, Fonctions et Operateurs ------------------------------------

!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
!contains


endmodule MATERIAU

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai   2002 (v0.0.1b): création du module
! avril 2003          : spécification des types de matériaux (LIN, KNL, XMAT)
!------------------------------------------------------------------------------!

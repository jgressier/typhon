!------------------------------------------------------------------------------!
! MODULE : MATERIAU                                 Authors : J. Gressier
!                                                   Created : May 2002
! Fonction                                          Modif   : (see history)
!   Heat Transfer solver, Physical Model Definitions
!
!------------------------------------------------------------------------------!
module MATERIAU

use TYPHMAKE     ! definition de la precision des reels
use MATER_LOI    ! definition generale d'une loi de variation des parametres

implicit none

! -- Constants  -------------------------------------------

real(krp), parameter :: stefan_cst = 5.6703E-8_krp

! -- Parameters -------------------------------------------

character, parameter :: mat_LIN  = 'L'   ! materiau a proprietes constantes
character, parameter :: mat_KNL  = 'K'   ! materiau a cp constant et conductivite non lineaire
character, parameter :: mat_XMAT = 'X'   ! materiau a proprietes specifiques

!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! ST_MATERIAU : structure pour la definition des equations
!------------------------------------------------------------------------------!
type st_materiau
  character(len=30) :: nom       ! nom du materiau
  character         :: type      ! cf constantes
  real(krp)         :: Cp        ! Capacite calorifique
  type(st_loi)      :: Energie   ! Energie (fct de temperature)
  type(st_loi)      :: Kd        ! conductivite thermique 
endtype st_materiau


! -- INTERFACES -------------------------------------------------------------


! -- Procedures, Fonctions et Operateurs ------------------------------------

!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
!contains


endmodule MATERIAU

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai   2002 (v0.0.1b): creation du module
! avril 2003          : specification des types de materiaux (LIN, KNL, XMAT)
!------------------------------------------------------------------------------!

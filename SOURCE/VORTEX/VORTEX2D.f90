!------------------------------------------------------------------------------!
! MODULE : VORTEX2D                       Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour le calcul
!   de singularites VORTEX 2D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module VORTEX2D

use TYPHMAKE     ! declaration des precisions
use MATH         ! constantes et fonctions mathematiques
use GEO2D        ! module de gestion de vecteurs 2D

! -- DECLARATIONS -----------------------------------------------------------

real(krp), parameter :: i2pi = .5_krp/pi      !   1/(2*pi)

! -- types --

type sng_vortex2d
  type(v2d) :: center
  real(krp) :: intensity
endtype

! -- INTERFACES -------------------------------------------------------------

interface vel_induced
  module procedure vel_induc_vort2d
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : vel_induc_vort2d
!------------------------------------------------------------------------------!
type(v2d) function vel_induc_vort2d(pos, vort)
implicit none
type(v2d)          :: pos     ! position de la vitesse induite
type(sng_vortex2d) :: vort    ! source   de la vitesse induite

  type(v2d) :: CM    ! distance source/position

  CM = pos - vort%center
  vel_induc_vort2d = i2pi * vort%intensity / abs(CM)**2 * rot(CM)

endfunction vel_induc_vort2d


endmodule VORTEX2D

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation du module
!------------------------------------------------------------------------------!

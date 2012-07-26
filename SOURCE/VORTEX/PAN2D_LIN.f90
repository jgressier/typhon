!------------------------------------------------------------------------------!
! MODULE : PAN2D_LIN                      Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour le calcul
!   de distribution LINEAIRE de singularite sur PANNEAU 2D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module PAN2D_LIN

use TYPHMAKE     ! declaration des precisions
use VEC2D        ! module de gestion de vecteurs 2D
use GEO3D
use VORTEX2D

! -- DECLARATIONS -----------------------------------------------------------

! -- types de singularites sur les panneaux --
integer, parameter :: sng_vortexlin = 10  ! repartition lineaire de vorticite

! -- types --

type pan2dlin
  type(v2d) :: center, normal
  real(krp) :: intensity
endtype

! -- INTERFACES -------------------------------------------------------------

interface vel_induced
!  module procedure vel_induc_p2d_lin
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : vel_induc_pvortlin2d
! !! DEV !! vecteur 3D a modeliser en 2D + test CPU
!------------------------------------------------------------------------------!
type(v3d) function vel_induc_pvortlin2d(x, x1, x2, g1, g2)
implicit none
type(v3d)      :: x        ! position  de l'evaluation
type(v3d)      :: x1, x2   ! positions des sommets du panneau et normale
real(krp)      :: g1, g2   ! intensite de vortex aux extremites du panneau
! internes
type(v3d)      :: r1, r2, dx
type(v2d)      :: dp, vi
real(krp)      :: dth, af, d1, d2, dl, dgdl, lndr, up, vp

  ! preparation du calcul
  r1  = x  - x1
  r2  = x  - x2
  dx  = x2 - x1
  dth = atan2(r2%y*r1%x-r2%x*r1%y, r1%x*r2%x+r1%y*r2%y)
  af  = atan2(dx%y, dx%x)
  d1  = abs(r1)
  d2  = abs(r2)
  dl = xcomp(rot(v2d(dx%x, dx%y), -af))
  !print*,'test dl:',dl, abs(dx)
  !dl = abs(dx)
  dp = rot(v2d(r1%x, r1%y), -af)
  dgdl = (g2 - g1) / dl
  lndr = log(d2/d1)

  !print*,'vel_induc:',real((/g1,dgdl,dp%x,dth/),4)
  ! vitesse induite dans le repere de la face (panneau)
  vi%x = -i2pi*(dp%y*dgdl*lndr + (g1 + dgdl*dp%x)*dth)
  vi%y = -i2pi*((g1+dgdl*dp%x)*lndr + g2-g1 - dp%y*dgdl*dth)

  ! rotation du vecteur vitesse induit dans le plan d'origine
  vel_induc_pvortlin2d = v3d_of(rot(vi, af))

endfunction vel_induc_pvortlin2d


!------------------------------------------------------------------------------!
! Procedure : coef_induc_pvortlin2d
! Calcul des coefficients d'effets des singularites sur la vitesse normale V.n
! !! DEV !! vecteur 3D a modeliser en 2D + test CPU
!------------------------------------------------------------------------------!
subroutine coef_induc_pvortlin2d(x, n, x1, x2, c1, c2)
implicit none
type(v3d)      :: x, n     ! position  de l'evaluation et normale
type(v3d)      :: x1, x2   ! positions des sommets du panneau et normale
real(krp)      :: c1, c2   ! coefficients d'effets des singularites
! internes
type(v3d)      :: r1, r2, dx
type(v2d)      :: dp, vi
real(krp)      :: dth, af, d1, d2, dl, dgdl, lndr, up, vp

  ! preparation du calcul
  r1  = x  - x1
  r2  = x  - x2
  dx  = x2 - x1
  dth = atan2(r2%y*r1%x-r2%x*r1%y, r1%x*r2%x+r1%y*r2%y)
  af  = atan2(dx%y, dx%x)
  d1  = abs(r1)
  d2  = abs(r2)
  dl = xcomp(rot(v2d(dx%x, dx%y), -af))
  !dl  = abs(dx)
  dp  = rot(v2d(r1%x, r1%y), -af)
  lndr = log(d2/d1)

  ! vitesse induite dans le repere de la face (panneau) par g1 unitaire
  vi%x = -i2pi*(-dp%y/dl*lndr + (1._krp - dp%x/dl)*dth)
  vi%y = -i2pi*((1._krp - dp%x/dl)*lndr -1._krp + dp%y/dl*dth)

  ! coefficient c1 : vitesse normale induite par g1 unitaire
  c1 = v3d_of(rot(vi, af)).scal.n

  ! vitesse induite dans le repere de la face (panneau) par g2 unitaire
  vi%x = -i2pi*(dp%y/dl*lndr + dp%x/dl*dth)
  vi%y = -i2pi*(dp%x/dl*lndr + 1._krp - dp%y/dl*dth)

  ! coefficient c2 : vitesse normale induite par g2 unitaire
  c2 = v3d_of(rot(vi, af)).scal.n

endsubroutine coef_induc_pvortlin2d


endmodule PAN2D_LIN

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation du module (vel + coef induits par panneau vortex lineaire)
! avr  2004 : correction et validation des procedures
!------------------------------------------------------------------------------!

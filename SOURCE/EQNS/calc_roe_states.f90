!------------------------------------------------------------------------------!
! Procedure : calc_roe_states              Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Computation of HLLE flux for Euler equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_roe_states(fluid, nflux, cell_l, cell_r, roe)

use TYPHMAKE
use VARCOM
use EQNS
use GEO3D

implicit none

! -- Declaration des entrées --
type(st_espece)       :: fluid            ! paramètres de définition du fluide
integer               :: nflux            ! nombre de flux (face) à calculer
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives

! -- Declaration des sorties --
type(st_nsetat), dimension(1:nflux) &
                      :: roe              ! roe state average

! -- Declaration des variables internes --
integer               :: if
real(krp)             :: gig1
real(krp)             :: frac, kl, kr     ! weights of roe average
real(krp)             :: hil, hir, him    ! mass total enthalpy

! -- Debut de la procedure --

gig1 = fluid%gamma / (fluid%gamma - 1._krp)

do if = 1, nflux
  frac = sqrt(cell_r(if)%density / cell_l(if)%density)
  kl   = 1._krp / (1._krp + frac)
  kr   = frac*kl
  hil  = gig1*cell_l(if)%pressure/cell_l(if)%density + .5_krp*sqrabs(cell_l(if)%velocity)
  hir  = gig1*cell_r(if)%pressure/cell_r(if)%density + .5_krp*sqrabs(cell_r(if)%velocity)
  him  = kl*hil + kr*hir 
  roe(if)%density  = cell_l(if)%density*frac
  roe(if)%velocity = kl*cell_l(if)%velocity + kr*cell_r(if)%velocity
  roe(if)%pressure = (him -  .5_krp*sqrabs(roe(if)%velocity))*roe(if)%density/gig1
enddo

endsubroutine calc_roe_states

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation, Roe average state computation
!------------------------------------------------------------------------------!

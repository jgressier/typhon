!------------------------------------------------------------------------------!
! Procedure : calc_flux_ale                 Authors : A. Gardi
!
! Function
!   Computation of ALE flux for NS equations
!
!------------------------------------------------------------------------------!
subroutine  calc_flux_ale(defsolver, nflux, ista, umesh, cg_l, cg_r, QL, QR, flux, calc_jac, jacL, jacR)

use TYPHMAKE
use MENU_SOLVER
use USTMESH
use DEFFIELD
use EQNS
use GEO3D
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! solver parameters
integer               :: ista             ! index of first flux
integer               :: nflux            ! number of fluxes
type(st_ustmesh)      :: umesh
type(v3d),dimension(1:nflux) :: cg_l,cg_r ! cell centers (index related to face number)
type(st_nsetat)       :: QL, QR           ! primitive variables array
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Outputs --

! -- Internal variables --
integer               :: if, if_abs
real(krp)             :: id
type(v3d)             :: face_velocity
real(krp), dimension(1:nflux) :: dHL, dHR, rhoL,rhoR,rhoH,prL,prR,prH,energy
type(v3d), dimension(1:nflux) :: vL, vR, velH, momentum

! -- Body --

! -- Pre-processing --
select case(defsolver%defale%type)

case(ale_none)

case default
! PART COPIED FROM calc_flux_viscous, maybe it should be merged and placed in a new shared subroutine?
do if = 1, nflux
  if_abs = ista-1+if
  dHL(if) = abs(umesh%mesh%iface(if_abs, 1, 1)%centre - cg_l(if))
  dHR(if) = abs(umesh%mesh%iface(if_abs, 1, 1)%centre - cg_r(if))
  id      = 1._krp/(dHL(if) + dHR(if))
  dHL(if) = id*dHL(if)
  dHR(if) = id*dHR(if)
enddo

vL(1:nflux) = QL%velocity(1:nflux)
vR(1:nflux) = QR%velocity(1:nflux)
velH(1:nflux) = dHR(1:nflux)*vL(1:nflux) + dHL(1:nflux)*vR(1:nflux)

rhoL(1:nflux) = QL%density(1:nflux)
rhoR(1:nflux) = QR%density(1:nflux)
rhoH(1:nflux) = dHR(1:nflux)*rhoL(1:nflux) + dHL(1:nflux)*rhoR(1:nflux)

prL(1:nflux) = QL%pressure(1:nflux)
prR(1:nflux) = QR%pressure(1:nflux)
prH(1:nflux) = dHR(1:nflux)*prL(1:nflux) + dHL(1:nflux)*prR(1:nflux)

momentum(1:nflux) = rhoH(1:nflux)*velH(1:nflux)
energy(1:nflux) = prH(1:nflux) + 0.5*rhoH(1:nflux)*(sqrabs(velH(1:nflux)))
! ENDPART

! FLUX EVALUATION
do if = 1, nflux
  if_abs = ista-1+if
  
  ! mass flux
  flux%tabscal(1)%scal(if_abs) = flux%tabscal(1)%scal(if_abs) + (defsolver%defale%face_velocity(if_abs) .scal. umesh%mesh%iface(if_abs, 1, 1)%normale) * rhoH(if)

  ! momentum flux
  flux%tabvect(1)%vect(if_abs) = flux%tabvect(1)%vect(if_abs) + (defsolver%defale%face_velocity(if_abs) .scal. umesh%mesh%iface(if_abs, 1, 1)%normale) * momentum(if)

  ! energy flux
  flux%tabscal(2)%scal(if_abs) = flux%tabscal(2)%scal(if_abs) + (defsolver%defale%face_velocity(if_abs) .scal. umesh%mesh%iface(if_abs, 1, 1)%normale) * energy(if)
enddo

endselect

endsubroutine calc_flux_ale

!------------------------------------------------------------------------------!
! Changes history
!
! Mar 2011 : creation, ALE flux
!------------------------------------------------------------------------------!

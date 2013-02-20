!------------------------------------------------------------------------------!
! Procedure : calc_flux_viscous_face
!
! Function
!   Computation of VISCOUS flux for NS equations with SVM method on gauss points
!
!------------------------------------------------------------------------------!
subroutine calc_flux_viscous_face(defsolver, defspat, nflux, ista, face, cg_l, cg_r, &
                             QL, QR, gradL, gradR, flux)
use TYPHMAKE
use OUTPUT
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D
use TENSOR3
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! solver parameters
type(mnu_spat)        :: defspat          ! space integration parameters
integer               :: nflux            ! number of fluxes
integer               :: ista             ! index of first flux/gradients
type(st_face), dimension(1:nflux) &
                      :: face             ! geom. data of faces
type(v3d),     dimension(1:nflux) &
                      :: cg_l, cg_r       ! cell centers (index related to face number)
type(st_nsetat)       :: QL, QR           ! primitive variables array
type(st_genericfield) :: gradL, gradR     ! left & right gradients
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
real(krp), parameter      :: theta = 1._krp
real(krp), dimension(nflux) :: dHR, dHL   ! cell to face distance
type(v3d), dimension(nflux) :: vLR        ! cell to cell vector
type(t3d), dimension(nflux) :: gradvH     ! face (H) vector (velocity gradient)
type(v3d), dimension(nflux) :: velH       ! face (H) velocity
type(v3d), dimension(nflux) :: vL, vR     ! left, right velocities
type(v3d), dimension(nflux) :: gradTL, gradTR  ! left, right temp grad
real(krp), dimension(nflux) :: TL, TR     ! left, right temperatures
real(krp), dimension(nflux) :: TH , gradTH       ! temperature at face center
real(krp), dimension(nflux) :: rhoH       ! density     at face center
real(krp), dimension(nflux) :: mu         ! viscosity   at face center
type(t3d)  :: sigma                               ! viscous tensor
type(v3d)  :: sigma_n
real(krp)  :: sigma_vn, addvisc
real(krp)  :: r_PG, cp, conduct
real(krp)  :: id
integer    :: if, i, iend

! -- BODY --

iend = ista-1+nflux

! -- Pre-processing --
r_PG = defsolver%defns%properties(1)%r_const        ! perfect gas constant
cp   = defsolver%defns%properties(1)%gamma * r_PG / &
      (defsolver%defns%properties(1)%gamma - 1)      ! heat capacity

!-------------------

vL(1:nflux) = QL%velocity(1:nflux)
vR(1:nflux) = QR%velocity(1:nflux)
velH(1:nflux) = 0.5_krp *(vL(1:nflux) + vR(1:nflux))

TL(1:nflux)   = QL%pressure(1:nflux) / (QL%density(1:nflux) * r_PG)
TR(1:nflux)   = QR%pressure(1:nflux) / (QR%density(1:nflux) * r_PG)
TH(1:nflux)   = 0.5_krp * (TL(1:nflux) + TR(1:nflux))
rhoH(1:nflux) = 0.5_krp *(QL%density(1:nflux) + QR%density(1:nflux))

! computation of temperature gradient :
! grad(T) = 1 / (density * r) * grad(P) - P/(r * density**2) * grad(density)
gradTL(1:nflux) = 1._krp/(QL%density(1:nflux) * r_PG) * gradL%tabvect(2)%vect(ista:iend) - &
     QL%pressure(1:nflux) / (QL%density(1:nflux)**2 * r_PG) * &
     gradL%tabvect(1)%vect(ista:iend)
gradTR(1:nflux) = 1._krp/(QR%density(1:nflux) * r_PG) * gradR%tabvect(2)%vect(ista:iend) - &
     QR%pressure(1:nflux) / (QR%density(1:nflux)**2 * r_PG) * &
     gradR%tabvect(1)%vect(ista:iend)

!enddo

call calc_viscosity(defsolver%defns%properties(1), rhoH(1:nflux), TH(1:nflux), mu(1:nflux))

! velocity gradient at the face
gradvH(1:nflux) = 0.5_krp *(gradL%tabtens(1)%tens(ista:iend)+gradR%tabtens(1)%tens(ista:iend))

! viscous, heat flux
! Flux = (sigma . V) . normal + conductivity . grad (T) . normal
! sigma : viscous stress tensor

do if = 1, nflux

  ! viscous stress tensor
  sigma = gradvH(if) + t3d_transp(gradvH(if))
  addvisc = - (2._krp/3._krp)*t3d_trace(gradvH(if))
  call t3d_adddiag(sigma, addvisc)
  sigma    = mu(if)*sigma
  sigma_n  = sigma.scal.face(if)%normale
  sigma_vn = sigma_n.scal.velH(if)

! temperature gradient at the face   
  gradTH(if)  = (0.5_krp *gradTL(if) + 0.5_krp *gradTR(if)).scal.face(if)%normale


  ! momentum flux
  flux%tabvect(1)%vect(ista-1+if) = flux%tabvect(1)%vect(ista-1+if) - (sigma_n)

  ! energy flux
  flux%tabscal(2)%scal(ista-1+if) = flux%tabscal(2)%scal(ista-1+if) - sigma_vn

  ! heat conduction term
  ! thermal conductivity
  conduct = mu(if) * cp / defsolver%defns%properties(1)%prandtl
  flux%tabscal(2)%scal(ista-1+if) = flux%tabscal(2)%scal(ista-1+if)  &
                                     - conduct * gradTH(if)

enddo

endsubroutine calc_flux_viscous_face

!------------------------------------------------------------------------------!
! Changes history
! Feb  2013: created 
!------------------------------------------------------------------------------!

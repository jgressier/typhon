!------------------------------------------------------------------------------!
! Procedure : calc_flux_viscous                 Authors : J. Gressier
!
! Function
!   Computation of VISCOUS flux for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_viscous(defsolver, defspat, nflux, ideb, face, cg_l, cg_r, &
                             QL, QR, gradL, gradR, flux,    &
                             calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
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
integer               :: ideb             ! index of first flux
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
real(krp), dimension(taille_buffer) :: dHR, dHL   ! cell to face distance
type(v3d), dimension(taille_buffer) :: vLR        ! cell to cell vector
type(t3d), dimension(taille_buffer) :: gradvH     ! face (H) vector (velocity gradient)
type(v3d), dimension(taille_buffer) :: velH       ! face (H) velocity
type(v3d), dimension(taille_buffer) :: vL, vR     ! left, right velocities
type(v3d), dimension(taille_buffer) :: gradTL, gradTR  ! left, right temp grad
real(krp), dimension(taille_buffer) :: TL, TR     ! left, right temperatures
real(krp), dimension(taille_buffer) :: TH, gradTH ! temperature at face center
real(krp), dimension(taille_buffer) :: rhoH       ! density     at face center
real(krp), dimension(taille_buffer) :: mu         ! viscosity   at face center
type(t3d)  :: sigma                               ! viscous tensor
type(v3d)  :: sigma_n
real(krp)  :: sigma_vn, addvisc
real(krp)  :: r_PG, cp, conduct
real(krp)  :: id
integer    :: if, i

! -- Body --

!allocate( kH(nflux))    ! conductivite en H, centre de face
!allocate(dHR(nflux))    ! distance HR, rapportee a HL+HR
!allocate(dHL(nflux))    ! distance HL, rapportee a HL+HR
!allocate(dLR(nflux))    ! distance LR (difference de HR+HL)
!allocate(vLR(nflux))    ! vecteur  LR

! -- Pre-processing --
r_PG = defsolver%defns%properties(1)%r_const        ! perfect gas constant
cp = defsolver%defns%properties(1)%gamma * r_PG / &
     (defsolver%defns%properties(1)%gamma - 1)      ! heat capacity

do if = 1, nflux
  dHL(if) = abs(face(if)%centre - cg_l(if))
  dHR(if) = abs(face(if)%centre - cg_r(if))
  id      = 1._krp/(dHL(if) + dHR(if))
  dHL(if) = id*dHL(if)
  dHR(if) = id*dHR(if)
enddo

vLR(1:nflux) = cg_r(1:nflux) - cg_l(1:nflux)
  ! DEV / OPT : calcul de la distance au carre si c'est la seule utilisee
  ! pour eviter sqrt()**2
  !dLR(if) = abs(vLR(if))

vL(1:nflux) = QL%velocity(1:nflux)
vR(1:nflux) = QR%velocity(1:nflux)
velH(1:nflux) = dHR(1:nflux)*vL(1:nflux) + dHL(1:nflux)*vR(1:nflux)

TL(1:nflux)   = QL%pressure(1:nflux) / (QL%density(1:nflux) * r_PG)
TR(1:nflux)   = QR%pressure(1:nflux) / (QR%density(1:nflux) * r_PG)
TH(1:nflux)   = dHR(1:nflux)*TL(1:nflux) + dHL(1:nflux)*TR(1:nflux)
rhoH(1:nflux) = dHR(1:nflux)*QL%density(1:nflux) + dHL(1:nflux)*QR%density(1:nflux)

! computation of temperature gradient :
! grad(T) = 1 / (density * r) * grad(P) - P/(r * density**2) * grad(density)
gradTL(1:nflux) = 1._krp/(QL%density(1:nflux) * r_PG) * gradL%tabvect(2)%vect(1:nflux) - &
     QL%pressure(1:nflux) / (QL%density(1:nflux)**2 * r_PG) * &
     gradL%tabvect(1)%vect(1:nflux)
gradTR(1:nflux) = 1._krp/(QR%density(1:nflux) * r_PG) * gradR%tabvect(2)%vect(1:nflux) - &
     QR%pressure(1:nflux) / (QR%density(1:nflux)**2 * r_PG) * &
     gradR%tabvect(1)%vect(1:nflux)

!enddo

call calc_viscosity(defsolver%defns%properties(1), rhoH(1:nflux), TH(1:nflux), mu(1:nflux))

! velocity gradient at the face
call interp_facegradient_vect(nflux, defspat%sch_dis, dHL, dHR, vLR, vL, vR,&
     gradL%tabtens(1)%tens,gradR%tabtens(1)%tens, gradvH)

! temperature gradient at the face
call interp_facegradn_scal(nflux, defspat%sch_dis, dHL, dHR, vLR, face, TL, TR,&
     gradTL, gradTR, gradTH)

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

  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = flux%tabvect(1)%vect(ideb-1+if) - (sigma_n)

  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = flux%tabscal(2)%scal(ideb-1+if) - sigma_vn

  ! heat conduction term
  ! thermal conductivity
  conduct = mu(if) * cp / defsolver%defns%properties(1)%prandtl
  flux%tabscal(2)%scal(ideb-1+if) = flux%tabscal(2)%scal(ideb-1+if)  &
                                     - conduct * gradTH(if)

enddo

!--------------------------------------------------------------
! Jacobian calculation
!--------------------------------------------------------------
if (calc_jac) then
  do if = 1, nflux
    id = mu(if) / (dHR(if)*QL%density(if) + dHL(if)*QR%density(if)) &
                / abs(cg_r(if) - cg_l(if))
    do i = 1, 5
      jacR%mat(i,i,if) = jacR%mat(i,i,if) - id
      jacL%mat(i,i,if) = jacL%mat(i,i,if) + id
    enddo
  enddo
endif

!deallocate()


endsubroutine calc_flux_viscous

!------------------------------------------------------------------------------!
! Changes history
!
! Feb 2005 : creation, VISCOUS flux
!------------------------------------------------------------------------------!

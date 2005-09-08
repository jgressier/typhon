!------------------------------------------------------------------------------!
! Procedure : calc_flux_viscous                  Authors : J. Gressier
!                                                Created : February 2004
! Fonction                                       Modif   : (cf history)
!   Computation of VISCOUS flux for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_viscous(defsolver, defspat, nflux, ideb, face, cg_l, cg_r, &
                             cell_l, cell_r, gradL, gradR, flux,    &
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

! -- Declaration des entrees --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! geom. data of faces
type(v3d),     dimension(1:nflux) &
                      :: cg_l, cg_r       ! cell centers (index related to face number)
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
type(st_genericfield) :: gradL, gradR     ! left & right gradients
logical               :: calc_jac         ! should compute jacobian matrices or not


! -- Declaration des sorties --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! jac associees

! -- Declaration des variables internes --
real(krp), parameter      :: theta = 1._krp
real(krp), dimension(taille_buffer) :: dHR, dHL   ! cell to face distance
type(v3d), dimension(taille_buffer) :: vLR        ! cell to cell vector
type(t3d), dimension(taille_buffer) :: gradvH     ! face (H) vector (velocity 
                                                  ! gradient)
type(v3d), dimension(taille_buffer) :: velH       ! face (H) velocity
type(v3d), dimension(taille_buffer) :: vL, vR     ! left, right velocities
type(v3d), dimension(taille_buffer) :: gradTL, gradTR  ! left, right temp grad
real(krp), dimension(taille_buffer) :: TL, TR     ! left, right temperatures
real(krp), dimension(taille_buffer) :: TH, mu, gradTH ! temperature en H
type(t3d)  :: sigma                               ! viscous tensor
type(v3d)  :: sigma_n
real(krp)  :: sigma_vn, addvisc
real(krp)  :: r_PG, cp, conduct
real(krp)  :: id
integer    :: if

! -- Debut de la procedure --

!allocate( kH(nflux))    ! conductivite en H, centre de face
!allocate(dHR(nflux))    ! distance HR, rapportee a HL+HR
!allocate(dHL(nflux))    ! distance HL, rapportee a HL+HR
!allocate(dLR(nflux))    ! distance LR (difference de HR+HL)
!allocate(vLR(nflux))    ! vecteur  LR

! -- Calculs preliminaires --
r_PG = defsolver%defns%properties(1)%r_const        ! perfect gas constant
cp = defsolver%defns%properties(1)%gamma * r_PG / &
     (defsolver%defns%properties(1)%gamma - 1)      ! heat capacity

do if = 1, nflux
  dHL(if) = abs(face(if)%centre - cg_l(if))
  dHR(if) = abs(face(if)%centre - cg_r(if))
  id      = 1._krp/(dHL(if) + dHR(if))
  dHL(if) = id*dHL(if) 
  dHR(if) = id*dHR(if) 
  vLR(if) = cg_r(if) - cg_l(if)
  ! DEV / OPT : calcul de la distance au carre si c'est la seule utilisee
  ! pour eviter sqrt()**2
  !dLR(if) = abs(vLR(if))

  vL(if) = cell_l(if)%velocity
  vR(if) = cell_r(if)%velocity
  velH(if) = dHR(if)*vL(if) + dHL(if)*vR(if)

  TL(if) = cell_l(if)%pressure / (cell_l(if)%density * r_PG)
  TR(if) = cell_r(if)%pressure / (cell_r(if)%density * r_PG)
  TH(if) = dHR(if)*TL(if) + dHL(if)*TR(if)

  ! computation of temperature gradient : 
  ! grad(T) = 1 / (density * r) * grad(P) - P/(r * density**2) * grad(density)
  gradTL(if) = 1._krp/(cell_l(if)%density * r_PG) * gradL%tabvect(2)%vect(if) - &
               cell_l(if)%pressure / (cell_l(if)%density**2 * r_PG) * &
               gradL%tabvect(1)%vect(if)
  gradTR(if) = 1._krp/(cell_r(if)%density * r_PG) * gradR%tabvect(2)%vect(if) - &
               cell_r(if)%pressure / (cell_r(if)%density**2 * r_PG) * &
               gradR%tabvect(1)%vect(if)

enddo

select case(defsolver%defns%typ_visc)
case(visc_suth)
  call calc_visc_suther(defsolver%defns, nflux, TH, mu, 1)
case(visc_cst)
  mu(1:nflux) = defsolver%defns%properties(1)%visc_dyn
case(visc_lin)
  mu(1:nflux) = defsolver%defns%properties(1)%visc_dyn*TH(1:nflux)
case default
  call erreur("viscosity computation","unknown kind of computation")
endselect

! velocity gradient at the face
call interp_facegradient_vect(nflux,defspat%sch_dis,dHL,dHR,vLR,vL,vR,&
     gradL%tabtens(1)%tens,gradR%tabtens(1)%tens,gradvH)

! temperature gradient at the face
call interp_facegradn_scal(nflux,defspat%sch_dis,dHL,dHR,vLR,face,TL,TR,&
     gradTL,gradTR,gradTH)

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
  flux%tabscal(2)%scal(ideb-1+if) = flux%tabscal(2)%scal(ideb-1+if) + &
                                     - sigma_vn

  ! heat conduction term
  ! thermal conductivity
  conduct = mu(if) * cp / defsolver%defns%properties(1)%prandtl
  flux%tabscal(2)%scal(ideb-1+if) = flux%tabscal(2)%scal(ideb-1+if) + &
                                     - conduct * gradTH(if)

enddo

!!$!--------------------------------------------------------------
!!$! Calcul des jacobiennes
!!$!--------------------------------------------------------------
!!$if (calc_jac) then
!!$  call erreur("Developpement","Calcul de jacobiennes du flux VISCOUS non implemente")
!!$endif
!!$!  do if = 1, nflux
!!$!    jacR(if) =  - kH(if) * (vLR(if).scal.face(if)%normale) &
!!$!                  / (defsolver%defkdif%materiau%Cp * dLR(if)**2)
!!$!    jacL(if) = -jacR(if)
!!$!  enddo
!!$!endif


!deallocate()


endsubroutine calc_flux_viscous

!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : creation, VISCOUS flux
!------------------------------------------------------------------------------!

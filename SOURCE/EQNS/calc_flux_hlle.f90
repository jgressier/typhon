!------------------------------------------------------------------------------!
! Procedure : calc_flux_hlle              Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Computation of HLLE flux for Euler equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hlle(defsolver, defspat, nflux, face,        &
                          cg_l, cell_l, cg_r, cell_r, flux, ideb, &
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

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: defsolver        ! paramètres de définition du solveur
type(mnu_spat)        :: defspat          ! paramètres d'intégration spatiale
integer               :: nflux            ! nombre de flux (face) à calculer
integer               :: ideb             ! indice du premier flux à remplir
type(st_face),     dimension(1:nflux) & 
                      :: face             ! données géométriques des faces
type(v3d),         dimension(1:nflux) &
                      :: cg_l, cg_r       ! centres des cellules
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Declaration des sorties --
type(st_genericfield)        :: flux
real(krp), dimension(nflux)  :: jacL, jacR  ! jac associées

! -- Declaration des variables internes --
integer                   :: if
type(st_nsetat), dimension(:), allocatable &
                          :: roe
type(v3d)                 :: fn
real(krp)                 :: g, ig1, sl, sr, iks, kl, kr, ku
real(krp)                 :: am, al, ar, vm, vnl, vnr, rel, rer


! -- Debut de la procedure --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Calculs préliminaires --

allocate(roe(1:nflux))

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

! -- Calcul du flux --

do if = 1, nflux

  fn  = face(if)%normale 
  vnl = cell_l(if)%velocity.scal.fn                    ! face normal velocity (left  state)
  vnr = cell_r(if)%velocity.scal.fn                    !                      (right state)
  vm  =    roe(if)%velocity.scal.fn                    !                      (roe average state)
  al  = sqrt(g*cell_l(if)%pressure/cell_l(if)%density) ! sound speed          (left state)
  ar  = sqrt(g*cell_r(if)%pressure/cell_r(if)%density) !                      (right state)
  am  = sqrt(g*   roe(if)%pressure/   roe(if)%density) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l(if)%pressure + .5_krp*cell_l(if)%density*sqrabs(cell_l(if)%velocity)
  rer = ig1*cell_r(if)%pressure + .5_krp*cell_r(if)%density*sqrabs(cell_r(if)%velocity)

  sl  = min(0._krp, vnl-al, vm-am)                  ! left  highest wave speed
  sr  = max(0._krp, vnr+ar, vm+am)                  ! right highest wave speed
  iks = 1._krp/(sr-sl)
  kl  =  sr*iks
  kr  = -sl*iks
  ku  = -sl*sr*iks

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = (kl*vnl-ku)*cell_l(if)%density + (kr*vnr+ku)*cell_r(if)%density
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = (kl*vnl-ku)*rel + (kr*vnr+ku)*rer &
                                  + (kl*cell_l(if)%pressure - kr*cell_r(if)%pressure)
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = ((kl*vnl-ku)*cell_l(if)%density)*cell_l(if)%velocity &
                                  + ((kr*vnr+ku)*cell_r(if)%density)*cell_r(if)%velocity &
                                  + (kl*cell_l(if)%pressure - kr*cell_r(if)%pressure)*fn
enddo

deallocate(roe)

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then
  call erreur("Développement","Calcul de jacobiennes du flux HLLE non implémenté")
endif
!  do if = 1, nflux
!    jacR(if) =  - kH(if) * (vLR(if).scal.face(if)%normale) &
!                  / (defsolver%defkdif%materiau%Cp * dLR(if)**2)
!    jacL(if) = -jacR(if)
!  enddo
!endif


!deallocate()


endsubroutine calc_flux_hlle

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation, HLLE flux
!------------------------------------------------------------------------------!

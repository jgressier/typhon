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
                          cell_l, cell_r, flux, ideb,             &
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
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal variables --
integer                     :: if
type(st_nsetat), dimension(:), allocatable &
                            :: roe
type(v3d)                   :: fn
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr
real(krp)                   :: g, ig1, iks, kl, kr, ku
real(krp)                   :: am, al, ar, vm, rel, rer


! -- Debut de la procedure --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Calculs preliminaires --

allocate(roe(1:nflux))

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

! -- Calcul du flux --

do if = 1, nflux

  fn  = face(if)%normale 
  vnl(if) = cell_l(if)%velocity.scal.fn                ! face normal velocity (left  state)
  vnr(if) = cell_r(if)%velocity.scal.fn                !                      (right state)
  vm  = roe(if)%velocity.scal.fn                       !                      (roe average state)
  al  = sqrt(g*cell_l(if)%pressure/cell_l(if)%density) ! sound speed          (left state)
  ar  = sqrt(g*cell_r(if)%pressure/cell_r(if)%density) !                      (right state)
  am  = sqrt(g*   roe(if)%pressure/   roe(if)%density) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l(if)%pressure + .5_krp*cell_l(if)%density*sqrabs(cell_l(if)%velocity)
  rer = ig1*cell_r(if)%pressure + .5_krp*cell_r(if)%density*sqrabs(cell_r(if)%velocity)

  sl(if) = min(0._krp, vnl(if)-al, vm-am)              ! left  highest wave speed
  sr(if) = max(0._krp, vnr(if)+ar, vm+am)              ! right highest wave speed

  iks    = 1._krp/(sr(if)-sl(if))
  kl     =  sr(if)*iks
  kr     = -sl(if)*iks
  ku     =  sl(if)*sr(if)*iks

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = (kl*vnl(if)-ku)*cell_l(if)%density + (kr*vnr(if)+ku)*cell_r(if)%density
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = (kl*vnl(if)-ku)*rel + (kr*vnr(if)+ku)*rer &
                                  + (kl*vnl(if)*cell_l(if)%pressure + kr*vnr(if)*cell_r(if)%pressure)
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = ((kl*vnl(if)-ku)*cell_l(if)%density)*cell_l(if)%velocity &
                                  + ((kr*vnr(if)+ku)*cell_r(if)%density)*cell_r(if)%velocity &
                                  + (kl*cell_l(if)%pressure + kr*cell_r(if)%pressure)*fn
enddo

deallocate(roe)

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with HLLE flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_hll)
    call calc_jac_hll(defsolver, defspat, nflux, face,          &
                      cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
  case(jac_hlldiag)
    call calc_jac_hlldiag(defsolver, defspat, nflux, face,          &
                          cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
  case default
    call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif


endsubroutine calc_flux_hlle

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation, HLLE flux
! Aug  2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

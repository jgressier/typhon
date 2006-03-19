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
type(st_nsetat)       :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal variables --
integer                     :: if
type(st_nsetat)             :: roe
type(v3d), dimension(taille_buffer) :: fn
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr
real(krp)                   :: g, ig1, iks, kl, kr, ku
real(krp)                   :: am, al, ar, vm, rel, rer


! -- Debut de la procedure --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Calculs preliminaires --

call new(roe, nflux)

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

! -- Calcul du flux --

do if = 1, nflux
  fn(if)  = face(if)%normale 
enddo

vnl(1:nflux) = cell_l%velocity(1:nflux).scal.fn(1:nflux)       ! face normal velocity (left  state)
vnr(1:nflux) = cell_r%velocity(1:nflux).scal.fn(1:nflux)       !                      (right state)

do if = 1, nflux

  vm  = roe%velocity(if).scal.fn(if)                   !                      (roe average state)
  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! sound speed          (left state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)
  am  = sqrt(g*   roe%pressure(if)/   roe%density(if)) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  sl(if) = min(0._krp, vnl(if)-al, vm-am)              ! left  highest wave speed
  sr(if) = max(0._krp, vnr(if)+ar, vm+am)              ! right highest wave speed

  iks    = 1._krp/(sr(if)-sl(if))
  kl     =  sr(if)*iks
  kr     = -sl(if)*iks
  ku     =  sl(if)*sr(if)*iks

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = (kl*vnl(if)-ku)*cell_l%density(if) + (kr*vnr(if)+ku)*cell_r%density(if)
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = (kl*vnl(if)-ku)*rel + (kr*vnr(if)+ku)*rer &
                                  + (kl*vnl(if)*cell_l%pressure (if)+ kr*vnr(if)*cell_r%pressure(if))
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = ((kl*vnl(if)-ku)*cell_l%density(if))*cell_l%velocity(if) &
                                  + ((kr*vnr(if)+ku)*cell_r%density(if))*cell_r%velocity(if) &
                                  + (kl*cell_l%pressure(if) + kr*cell_r%pressure(if))*fn(if)
enddo

call delete(roe)

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

!------------------------------------------------------------------------------!
! Procedure : calc_flux_hllc                       Authors : J. Gressier
!                                                  Created : July 2005
! Fonction
!   Computation of HLLC flux for Euler equations (Batten variant)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hllc(defsolver, defspat, nflux, face,        &
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

! -- INPUTS --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- OUTPUTS --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal variables --
integer                   :: if
type(st_nsetat), dimension(:), allocatable &
                          :: roe
type(v3d)                 :: fn, rvst
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr
real(krp)                 :: g, ig1, iks
real(krp)                 :: am, al, ar, vm, rel, rer, rqL, rqR
real(krp)                 :: Sst, rst, pst, rest

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Calculs preliminaires --

allocate(roe(1:nflux))

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)
! -- Calcul du flux --

do if = 1, nflux

  fn  = face(if)%normale 
  vnl(if) = cell_l(if)%velocity.scal.fn                    ! face normal velocity (left  state)
  vnr(if) = cell_r(if)%velocity.scal.fn                    !                      (right state)
  vm  =    roe(if)%velocity.scal.fn                    !                      (roe average state)
  al  = sqrt(g*cell_l(if)%pressure/cell_l(if)%density) ! sound speed          (left state)
  ar  = sqrt(g*cell_r(if)%pressure/cell_r(if)%density) !                      (right state)
  am  = sqrt(g*   roe(if)%pressure/   roe(if)%density) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l(if)%pressure + .5_krp*cell_l(if)%density*sqrabs(cell_l(if)%velocity)
  rer = ig1*cell_r(if)%pressure + .5_krp*cell_r(if)%density*sqrabs(cell_r(if)%velocity)

  sl(if)  = min(vnl(if)-al, vm-am)                  ! left  highest wave speed
  sr(if)  = max(vnr(if)+ar, vm+am)                  ! right highest wave speed

  !-----------------------------
  ! FULLY UPWIND
  if (sl(if) >= 0._krp) then 
    flux%tabscal(1)%scal(ideb-1+if) = vnl(if)*cell_l(if)%density             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnl(if)*(rel + cell_l(if)%pressure)    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnl(if)*cell_l(if)%density)*cell_l(if)%velocity &
                                    + cell_l(if)%pressure*fn             ! momentum flux
  !-----------------------------
  ! FULLY UPWIND
  elseif (sr(if) <= 0._krp) then
    flux%tabscal(1)%scal(ideb-1+if) = vnr(if)*cell_r(if)%density             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnr(if)*(rer + cell_r(if)%pressure)    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnr(if)*cell_r(if)%density)*cell_r(if)%velocity &
                                    + cell_r(if)%pressure*fn             ! momentum flux
  !-----------------------------
  ! COMPUTATION OF CONTACT WAVE
  else
    rqL = cell_l(if)%density*(sl(if) - vnl(if))
    rqR = cell_r(if)%density*(sr(if) - vnr(if))
    Sst = (rqR*vnr(if) - rqL*vnl(if) - cell_r(if)%pressure + cell_l(if)%pressure)/(rqR - rqL)
    
    if (Sst >= 0._krp) then
      iks  = 1._krp/(sl(if) - Sst)
      rst  = rqL*iks
      pst  = cell_l(if)%pressure - rqL*(vnl(if)-Sst)
      rvst = iks*( (rqL*cell_l(if)%velocity) + (pst-cell_l(if)%pressure)*fn )
      rest = iks*(rel*(sl(if)-vnl(if)) - cell_l(if)%pressure*vnl(if) + pst*Sst)
    else
      iks  = 1._krp/(sr(if) - Sst)
      rst  = rqR*iks
      pst  = cell_r(if)%pressure - rqR*(vnr(if)-Sst)
      rvst = iks*( (rqR*cell_r(if)%velocity) + (pst-cell_r(if)%pressure)*fn )
      rest = iks*(rer*(sr(if)-vnr(if)) - cell_r(if)%pressure*vnr(if) + pst*Sst)
    endif

    flux%tabscal(1)%scal(ideb-1+if) = Sst*rst                ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = Sst*(rest + pst)       ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (Sst*rvst) + (pst*fn)  ! momentum flux
  endif

enddo

deallocate(roe)

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then

  sl(1:nflux) = min(sl(1:nflux), 0._krp)
  sr(1:nflux) = max(sr(1:nflux), 0._krp)

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with HLLC flux")
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

endsubroutine calc_flux_hllc

!------------------------------------------------------------------------------!
! Changes history
!
! July 2005 : creation, HLLC flux
! Aug  2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

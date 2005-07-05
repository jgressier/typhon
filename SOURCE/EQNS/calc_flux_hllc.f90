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

implicit none

! -- Declaration des entrees --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Declaration des sorties --
type(st_genericfield)        :: flux
real(krp), dimension(nflux)  :: jacL, jacR  ! jac associees

! -- Declaration des variables internes --
integer                   :: if
type(st_nsetat), dimension(:), allocatable &
                          :: roe
type(v3d)                 :: fn, rvst
real(krp)                 :: g, ig1, sl, sr, iks
real(krp)                 :: am, al, ar, vm, vnl, vnr, rel, rer, rqL, rqR
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
  vnl = cell_l(if)%velocity.scal.fn                    ! face normal velocity (left  state)
  vnr = cell_r(if)%velocity.scal.fn                    !                      (right state)
  vm  =    roe(if)%velocity.scal.fn                    !                      (roe average state)
  al  = sqrt(g*cell_l(if)%pressure/cell_l(if)%density) ! sound speed          (left state)
  ar  = sqrt(g*cell_r(if)%pressure/cell_r(if)%density) !                      (right state)
  am  = sqrt(g*   roe(if)%pressure/   roe(if)%density) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l(if)%pressure + .5_krp*cell_l(if)%density*sqrabs(cell_l(if)%velocity)
  rer = ig1*cell_r(if)%pressure + .5_krp*cell_r(if)%density*sqrabs(cell_r(if)%velocity)

  sl  = min(vnl-al, vm-am)                  ! left  highest wave speed
  sr  = max(vnr+ar, vm+am)                  ! right highest wave speed

  !-----------------------------
  ! FULLY UPWIND
  if (sl >= 0._krp) then 
    flux%tabscal(1)%scal(ideb-1+if) = vnl*cell_l(if)%density             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnl*(rel + cell_l(if)%pressure)    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnl*cell_l(if)%density)*cell_l(if)%velocity &
                                    + cell_l(if)%pressure*fn             ! momentum flux
  !-----------------------------
  ! FULLY UPWIND
  elseif (sr <= 0._krp) then
    flux%tabscal(1)%scal(ideb-1+if) = vnr*cell_r(if)%density             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnr*(rer + cell_r(if)%pressure)    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnr*cell_r(if)%density)*cell_r(if)%velocity &
                                    + cell_r(if)%pressure*fn             ! momentum flux
  !-----------------------------
  ! COMPUTATION OF CONTACT WAVE
  else
    rqL = cell_l(if)%density*(sl - vnl)
    rqR = cell_r(if)%density*(sr - vnr)
    Sst = (rqR*vnr - rqL*vnl - cell_r(if)%pressure + cell_l(if)%pressure)/(rqR - rqL)
    
    if (Sst >= 0._krp) then
      iks  = 1._krp/(sl - Sst)
      rst  = rqL*iks
      pst  = cell_l(if)%pressure - rqL*(vnl-Sst)
      rvst = iks*( (rqL*cell_l(if)%velocity) + (pst-cell_l(if)%pressure)*fn )
      rest = iks*(rel*(sl-vnl) - cell_l(if)%pressure*vnl + pst*Sst)
    else
      iks  = 1._krp/(sr - Sst)
      rst  = rqR*iks
      pst  = cell_r(if)%pressure - rqR*(vnr-Sst)
      rvst = iks*( (rqR*cell_r(if)%velocity) + (pst-cell_r(if)%pressure)*fn )
      rest = iks*(rer*(sr-vnr) - cell_r(if)%pressure*vnr + pst*Sst)
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
  call erreur("Developpement","Calcul de jacobiennes du flux HLLC non implemente")
endif
!  do if = 1, nflux
!    jacR(if) =  - kH(if) * (vLR(if).scal.face(if)%normale) &
!                  / (defsolver%defkdif%materiau%Cp * dLR(if)**2)
!    jacL(if) = -jacR(if)
!  enddo
!endif


!deallocate()


endsubroutine calc_flux_hllc

!------------------------------------------------------------------------------!
! Changes history
!
! July 2005 : creation, HLLC flux
!------------------------------------------------------------------------------!

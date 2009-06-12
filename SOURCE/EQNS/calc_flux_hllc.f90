!------------------------------------------------------------------------------!
! Procedure : calc_flux_hllc                    Authors : J. Gressier
!
! Function
!   Computation of HLLC flux for Euler equations
!   (Batten variant)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hllc(defsolver, defspat, nflux, face, &
                          cell_l, cell_r, flux, ideb,      &
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
type(mnu_solver)      :: defsolver        ! solver parameters
type(mnu_spat)        :: defspat          ! space integration parameters
integer               :: nflux            ! number of fluxes
integer               :: ideb             ! index of first flux
type(st_face), dimension(1:nflux) &
                      :: face             ! geom. data of faces
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
integer                 :: if
type(st_nsetat)         :: roe
type(v3d)               :: fn, rvst
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr
real(krp)               :: g, ig1, iks
real(krp)               :: am, al, ar, vm, rel, rer, rqL, rqR
real(krp)               :: Sst, rst, pst, rest

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Pre-processing --

call new(roe, nflux)

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

!-------------------------------
! Flux computation

do if = 1, nflux

  fn      = face(if)%normale
  vnl(if) = cell_l%velocity(if).scal.fn                ! face normal velocity (left  state)
  vnr(if) = cell_r%velocity(if).scal.fn                !                      (right state)
  vm  =    roe%velocity(if).scal.fn                    !                      (roe average state)
  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! speed of sound       (left  state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)
  am  = sqrt(g*   roe%pressure(if)/   roe%density(if)) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  sl(if)  = min(vnl(if)-al, vm-am)                  ! left  highest wave speed
  sr(if)  = max(vnr(if)+ar, vm+am)                  ! right highest wave speed

  !-----------------------------
  ! FULLY UPWIND
  if (sl(if) >= 0._krp) then
    flux%tabscal(1)%scal(ideb-1+if) = vnl(if)*cell_l%density(if)             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnl(if)*(rel + cell_l%pressure(if))    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnl(if)*cell_l%density(if))*cell_l%velocity(if) &
                                    + cell_l%pressure(if)*fn             ! momentum flux
  !-----------------------------
  ! FULLY UPWIND
  elseif (sr(if) <= 0._krp) then
    flux%tabscal(1)%scal(ideb-1+if) = vnr(if)*cell_r%density(if)             ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = vnr(if)*(rer + cell_r%pressure(if))    ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (vnr(if)*cell_r%density(if))*cell_r%velocity(if) &
                                    + cell_r%pressure(if)*fn             ! momentum flux
  !-----------------------------
  ! COMPUTATION OF CONTACT WAVE
  else
    rqL = cell_l%density(if)*(sl(if) - vnl(if))
    rqR = cell_r%density(if)*(sr(if) - vnr(if))
    Sst = (rqR*vnr(if) - rqL*vnl(if) - cell_r%pressure(if) + cell_l%pressure(if))/(rqR - rqL)

    if (Sst >= 0._krp) then
      iks  = 1._krp/(sl(if) - Sst)
      rst  = rqL*iks
      pst  = cell_l%pressure(if) - rqL*(vnl(if)-Sst)
      rvst = iks*( (rqL*cell_l%velocity(if)) + (pst-cell_l%pressure(if))*fn )
      rest = iks*(rel*(sl(if)-vnl(if)) - cell_l%pressure(if)*vnl(if) + pst*Sst)
    else
      iks  = 1._krp/(sr(if) - Sst)
      rst  = rqR*iks
      pst  = cell_r%pressure(if) - rqR*(vnr(if)-Sst)
      rvst = iks*( (rqR*cell_r%velocity(if)) + (pst-cell_r%pressure(if))*fn )
      rest = iks*(rer*(sr(if)-vnr(if)) - cell_r%pressure(if)*vnr(if) + pst*Sst)
    endif

    flux%tabscal(1)%scal(ideb-1+if) = Sst*rst                ! mass flux
    flux%tabscal(2)%scal(ideb-1+if) = Sst*(rest + pst)       ! energy flux
    flux%tabvect(1)%vect(ideb-1+if) = (Sst*rvst) + (pst*fn)  ! momentum flux
  endif

enddo

call delete(roe)

!--------------------------------------------------------------
! Jacobian calculation
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
! Jul 2005 : creation, HLLC flux
! Aug 2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

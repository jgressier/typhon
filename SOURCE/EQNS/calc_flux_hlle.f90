!------------------------------------------------------------------------------!
! Procedure : calc_flux_hlle                    Authors : J. Gressier
!
! Function
!   Computation of HLLE flux for Euler equations
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hlle(defsolver, defspat, nflux, face, &
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
type(v3d), dimension(taille_buffer) :: fn
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr, al, ar, mnl, mnr
real(krp)               :: g, ig1, iks, kl, kr, ku
real(krp)               :: am, vm, rel, rer

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Pre-processing --

call new(roe, nflux)

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

!-------------------------------
! Flux computation

do if = 1, nflux
  fn(if)  = face(if)%normale
enddo

vnl(1:nflux) = cell_l%velocity(1:nflux).scal.fn(1:nflux)       ! face normal velocity (left  state)
vnr(1:nflux) = cell_r%velocity(1:nflux).scal.fn(1:nflux)       !                      (right state)
al(1:nflux)  = sqrt(g*cell_l%pressure(1:nflux)/cell_l%density(1:nflux)) ! speed of sound       (left  state)
ar(1:nflux)  = sqrt(g*cell_r%pressure(1:nflux)/cell_r%density(1:nflux)) !                      (right state)

do if = 1, nflux

  vm  = roe%velocity(if).scal.fn(if)                   !                      (roe average state)
  am  = sqrt(g*   roe%pressure(if)/   roe%density(if)) !                      (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  sl(if) = min(0._krp, vnl(if)-al(if), vm-am)              ! left  highest wave speed
  sr(if) = max(0._krp, vnr(if)+ar(if), vm+am)              ! right highest wave speed

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
! Jacobian calculation
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

  case default ! --- errors will be checked in calc_jac_gencall
    mnl(1:nflux) = vnl(1:nflux)/al(1:nflux)
    mnr(1:nflux) = vnr(1:nflux)/ar(1:nflux)
    call calc_jac_gencall(defsolver, defspat, nflux, face,          &
                          cell_l, cell_r,  mnl, mnr, al, ar, ideb, jacL, jacR)
  endselect

endif


endsubroutine calc_flux_hlle

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2004 : creation, HLLE flux
! Aug 2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calc_flux_efm                     Authors : Praveen. C
!
!> @brief Computation of Kinetic flux for Euler equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_efm (defsolver, defspat, nflux, fn, &
                          cell_l, cell_r, flux, ideb,      &
                          calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
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
type(v3d)             :: fn(1:nflux)      ! face normals
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
integer                 :: if
real(krp), dimension(nflux) :: sl, sr
real(krp)               :: vnl, vnr, al, ar
real(krp)               :: cl, cr
real(krp)               :: g, ig1, iks
real(krp)               :: rel, rer
real(krp)               :: Sst, rst, pst, rest
real(krp)               :: modml, modmr, kinwl, kinwr, kindl, kindr, f1, f2

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)
f1  = sqrt(0.5_krp*g)
f2  = 0.5_krp/sqrt(PIcst)

! -- Pre-processing --

!-------------------------------
! Flux computation

do if = 1, nflux

  vnl = cell_l%velocity(if).scal.fn(if)            ! face normal velocity (left  state)
  vnr = cell_r%velocity(if).scal.fn(if)            !                      (right state)
  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! speed of sound       (left  state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)

  cl  = al/f1                                           ! modified speed of sound for EFM (left  state)
  cr  = ar/f1                                           !                                 (right state)


  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  modml = vnl/cl
  modmr = vnr/cr


  ! error function
  kinwl = 0.5_krp*(1.0_krp + erf(modml))
  kinwr = 0.5_krp*(1.0_krp - erf(modmr))

  ! exponential function
  kindl =   f2*exp(-modml*modml)
  kindr = - f2*exp(-modmr*modmr)

  !-----------------------------
  ! numericalflux
  flux%tabscal(1)%scal(ideb-1+if) = cell_l%density(if) * ( vnl * kinwl + cl*kindl) + &
                                    cell_r%density(if) * ( vnr * kinwr + cr*kindr)
  flux%tabscal(2)%scal(ideb-1+if) = (rel + cell_l%pressure(if)) * vnl * kinwl + &
                                    (rer + cell_r%pressure(if)) * vnr * kinwr + &
                                    (rel + 0.5_krp*cell_l%pressure(if)) * cl*kindl + &
                                    (rer + 0.5_krp*cell_r%pressure(if)) * cr*kindr
  flux%tabvect(1)%vect(ideb-1+if) = (cell_l%pressure(if) * kinwl  + &
                                     cell_r%pressure(if) * kinwr ) * fn(if) + &
                                     (vnl * kinwl + cl*kindl) * cell_l%density(if) * cell_l%velocity(if) + &
                                     (vnr * kinwr + cr*kindr) * cell_r%density(if) * cell_r%velocity(if)
enddo

!--------------------------------------------------------------
! Jacobian calculation
!--------------------------------------------------------------
if (calc_jac) then

  !sl(1:nflux) = min(sl(1:nflux), 0._krp)
  !sr(1:nflux) = max(sr(1:nflux), 0._krp)

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with EFM flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_hll)
    call erreur("Development", "HLL jacobian matrices not available with EFM flux")
  case(jac_hlldiag)
    call erreur("Development", "HLL diagonal jacobian matrices not available with EFM flux")
  case default
    call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif


endsubroutine calc_flux_efm

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2008 : creation, Kinetic flux
!------------------------------------------------------------------------------!

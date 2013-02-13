!------------------------------------------------------------------------------!
! Procedure : calc_flux_hlle                    Authors : J. Gressier
!
! Function
!   Computation of HLLE flux for Euler equations
!   (+ beta, kinetic and kinetic-beta variant)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hlle(defsolver, defspat, nflux, face, &
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
type(st_face), dimension(1:nflux) &
                      :: face             ! geom. data of faces
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
integer               :: if
type(st_nsetat)       :: roe
type(v3d), dimension(nflux) :: fn
real(krp), dimension(nflux) :: sl, sr
real(krp), dimension(nflux) :: vnl, vnr, al, ar
real(krp), dimension(nflux) :: mnl, mnr
real(krp)             :: cl, cr
real(krp)             :: g, ig1, iks
real(krp)             :: am, vm
real(krp)             :: kl, kr, ku
real(krp)             :: rel, rer
real(krp)             :: beta
real(krp)             :: modml, modmr, kinwl, kinwr, kindl, kindr, f1, f2

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)
f1  = sqrt(0.5_krp*g)
f2  = 0.5_krp/sqrt(PIcst)

select case(defspat%sch_hyp)
case(sch_hlle, &
     sch_hllek)
  beta = 1._krp
case(sch_hlleb, &
     sch_hllekb)
  beta = sqrt((g - 1._krp)/(2*g))
case default
  call error_stop("internal error: numerical scheme not implemented (flux computation)")
endselect

! -- Pre-processing --

call new(roe, nflux)

call calc_roe_states(defsolver%defns%properties(1), nflux, cell_l, cell_r, roe)

!-------------------------------
! Flux computation

fn(1:nflux)  = face(1:nflux)%normale

vnl(1:nflux) = cell_l%velocity(1:nflux).scal.fn(1:nflux)                ! face normal velocity (left  state)
vnr(1:nflux) = cell_r%velocity(1:nflux).scal.fn(1:nflux)                !                      (right state)
al(1:nflux)  = sqrt(g*cell_l%pressure(1:nflux)/cell_l%density(1:nflux)) ! speed of sound       (left  state)
ar(1:nflux)  = sqrt(g*cell_r%pressure(1:nflux)/cell_r%density(1:nflux)) !                      (right state)

do if = 1, nflux

  !!fn  = face(if)%normale
  !!vnl = cell_l%velocity(if).scal.fn(if)                     ! face normal velocity (left  state)
  !!vnr = cell_r%velocity(if).scal.fn(if)                     !                      (right state)
  !!al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if))      ! speed of sound       (left  state)
  !!ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if))      !                      (right state)

  cl  = al(if)/f1                                           ! modified speed of sound for EFM (left  state)
  cr  = ar(if)/f1                                           !                                 (right state)

  vm  = roe%velocity(if).scal.fn(if)                        ! (roe average state)
  am  = sqrt(g*   roe%pressure(if)/   roe%density(if))      ! (roe average state)

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

select case(defspat%sch_hyp)
case(sch_hlle, &
     sch_hlleb)
  sl(if) = min(0._krp, vnl(if)-beta*al(if), vm-am)          ! left  highest wave speed
  sr(if) = max(0._krp, vnr(if)+beta*ar(if), vm+am)          ! right highest wave speed
case(sch_hllek, &
     sch_hllekb)
  modml = vnl(if)/cl - f1*beta
  modmr = vnr(if)/cr + f1*beta
  kinwl = 0.5_krp*(1.0_krp - erf(modml))
  kinwr = 0.5_krp*(1.0_krp + erf(modmr))
  kindl = - f2*exp(-modml*modml)
  kindr = + f2*exp(-modmr*modmr)
  sl(if) = min(cl*(modml*kinwl+kindl), vm-am)
  sr(if) = max(cr*(modmr*kinwr+kindr), vm+am)
endselect

  iks    = 1._krp/(sr(if)-sl(if))
  kl     =  sr(if)*iks
  kr     = -sl(if)*iks
  ku     =  sl(if)*sr(if)*iks

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = (kl*vnl(if)-ku)*cell_l%density(if) &
                                  + (kr*vnr(if)+ku)*cell_r%density(if)
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = (kl*vnl(if)-ku)*rel &
                                  + (kr*vnr(if)+ku)*rer &
                                  + (kl*vnl(if)*cell_l%pressure(if) &
                                  +  kr*vnr(if)*cell_r%pressure(if))
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = ((kl*vnl(if)-ku)*cell_l%density(if))*cell_l%velocity(if) &
                                  + ((kr*vnr(if)+ku)*cell_r%density(if))*cell_r%velocity(if) &
                                  + (kl*cell_l%pressure(if) &
                                  +  kr*cell_r%pressure(if))*fn(if)

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
  case default
  ! --- errors will be checked in calc_jac_gencall
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
! Sep 2012 : kinetic evaluations
! Feb  2013 : kinetic/beta evaluations for hllc and hlle
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calc_flux_ausmm                   Authors : J. Gressier
!
!> @brief Computation of van Leer/Hanel and EFM/Hanel flux for Euler equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_fvs_hwps(defsolver, defspat, nflux, fn, &
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
real(krp), dimension(nflux) &
                        :: al, ar, mp, mm, pp, pm, Hl, Hr, mnl, mnr
real(krp)               :: g, gig1, iks, km, kd

! -- Body --

g    = defsolver%defns%properties(1)%gamma
gig1 = g/(g - 1._krp)

! -- Pre-processing --

!-------------------------------
! Flux computation

! -- speed of sound --

al(1:nflux) = sqrt(g*cell_l%pressure(1:nflux)/cell_l%density(1:nflux)) ! speed of sound (left  state)
ar(1:nflux) = sqrt(g*cell_r%pressure(1:nflux)/cell_r%density(1:nflux)) !                (right state)

! -- Mach numbers --

do if = 1, nflux
  mnl(if) = (cell_l%velocity(if).scal.fn(if))/al(if)    ! face normal velocity (left  state)
  mnr(if) = (cell_r%velocity(if).scal.fn(if))/ar(if)    !                      (right state)
enddo

! -- Mach numbers functions and Pressure upwinding functions --

select case(defspat%sch_hyp)
case(sch_hwps_vleer)
  where (mnl(1:nflux) >= 1._krp)
    mp(1:nflux) = mnl(1:nflux)
    pp(1:nflux) = 1._krp
  elsewhere (mnl(1:nflux) <= -1._krp)
    mp(1:nflux) = 0._krp
    pp(1:nflux) = 0._krp
  elsewhere 
    mp(1:nflux) = .25_krp*(mnl(1:nflux)+1._krp)**2
    pp(1:nflux) = mp(1:nflux)*(2._krp-mnl(1:nflux))
  endwhere
  where (mnr(1:nflux) <= -1._krp)
    mm(1:nflux) = mnr(1:nflux)
    pm(1:nflux) = 1._krp
  elsewhere (mnr(1:nflux) >= 1._krp)
    mm(1:nflux) = 0._krp
    pm(1:nflux) = 0._krp
  elsewhere 
    mm(1:nflux) = -.25_krp*(mnr(1:nflux)-1._krp)**2
    pm(1:nflux) = -mm(1:nflux)*(2._krp+mnr(1:nflux))
  endwhere
case(sch_hwps_efm)
  km  = sqrt(0.5_krp*g)
  kd  = 1.0_krp/sqrt(2.0_krp*PIcst*g)
  pp(1:nflux) = 0.5_krp*(1.0_krp + erf(km*mnl(1:nflux)))
  pm(1:nflux) = 0.5_krp*(1.0_krp - erf(km*mnr(1:nflux)))
  mp(1:nflux) = mnl(1:nflux)*pp(1:nflux) + kd*exp(-(km*mnl(1:nflux))**2)
  mm(1:nflux) = mnr(1:nflux)*pm(1:nflux) - kd*exp(-(km*mnr(1:nflux))**2)
case default
  call error_stop("internal error: numerical scheme not implemented (flux computation)")
endselect

! -- massic total enthalpy (left and right) --

Hl(1:nflux) = gig1*cell_l%pressure(1:nflux)/cell_l%density(1:nflux) + .5_krp*sqrabs(cell_l%velocity(1:nflux))
Hr(1:nflux) = gig1*cell_r%pressure(1:nflux)/cell_r%density(1:nflux) + .5_krp*sqrabs(cell_r%velocity(1:nflux))

! -- FVS : precompute (rho Vn)+/- from M+/- 

mp(1:nflux) = mp(1:nflux)*al(1:nflux)*cell_l%density(1:nflux)
mm(1:nflux) = mm(1:nflux)*ar(1:nflux)*cell_r%density(1:nflux)

do if = 1, nflux

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = mp(if) + mm(if)
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = mp(if)*Hl(if) + mm(if)*Hr(if)
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = mp(if)*cell_l%velocity(if) &
                                  + mm(if)*cell_r%velocity(if) &
                                  + (pp(if)*cell_l%pressure(if) + pm(if)*cell_r%pressure(if))*fn(if)
enddo

!--------------------------------------------------------------
! Jacobian calculation
!--------------------------------------------------------------
if (calc_jac) then

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with AUSMM flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_hll)
    call erreur("Development", "HLL jacobian matrices not available with AUSMM flux")
    !call calc_jac_hll(defsolver, defspat, nflux, face,          &
    !                  cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
  case(jac_hlldiag)
    call erreur("Development", "HLL diagonal jacobian matrices not available with AUSMM flux")
    !call calc_jac_hlldiag(defsolver, defspat, nflux, face,          &
    !                      cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)

  case default ! --- errors will be checked in calc_jac_gencall
    call calc_jac_gencall(defsolver, defspat, nflux, fn,          &
                          cell_l, cell_r,  mnl, mnr, al, ar, ideb, jacL, jacR)

  endselect

endif


endsubroutine calc_flux_fvs_hwps
!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2010 : creation, FVS van Leer/Hanel and FVS EFMH schemes
!------------------------------------------------------------------------------!

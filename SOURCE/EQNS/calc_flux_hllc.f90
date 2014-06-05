!------------------------------------------------------------------------------!
! Procedure : calc_flux_hllc                    Authors : J. Gressier
!
! Function
!   Computation of HLLC (Batten variant) flux for Euler equations
!   (+ beta, kinetic and kinetic-beta variant)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hllc(defsolver, defspat, nflux, face, &
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
integer               :: if, nsim, ic, isim
type(st_nsetat)       :: roe
type(v3d)             :: rvst
type(v3d), dimension(nflux) :: fn
real(krp), dimension(nflux) :: sl, sr
real(krp), dimension(nflux) :: vnl, vnr, al, ar
real(krp), dimension(nflux) :: mnl, mnr
real(krp)             :: cl, cr
real(krp)             :: g, ig1, iks
real(krp)             :: am, vm
real(krp)             :: rel, rer
real(krp)             :: beta
real(krp)             :: modml, modmr, kinwl, kinwr, kindl, kindr, f1, f2
real(krp)             :: rqL, rqR
real(krp)             :: Sst, rst, pst, rest

! -- Body --
nsim = defsolver%nsim
g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)
f1  = sqrt(0.5_krp*g)
f2  = 0.5_krp/sqrt(PIcst)

select case(defspat%sch_hyp)
case(sch_hllc, &
     sch_hllck)
  beta = 1._krp
case(sch_hllcb, &
     sch_hllckb)
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
case(sch_hllc, &
     sch_hllcb)
  sl(if) = min(vnl(if)-beta*al(if), vm-am)          ! left  highest wave speed
  sr(if) = max(vnr(if)+beta*ar(if), vm+am)          ! right highest wave speed
case(sch_hllck, &
     sch_hllckb)
  modml = vnl(if)/cl - f1*beta
  modmr = vnr(if)/cr + f1*beta
  kinwl = 0.5_krp*(1.0_krp - erf(modml))
  kinwr = 0.5_krp*(1.0_krp + erf(modmr))
  kindl = - f2*exp(-modml*modml)
  kindr = + f2*exp(-modmr*modmr)
  sl(if) = min(cl*(modml*kinwl+kindl), vm-am)
  sr(if) = max(cr*(modmr*kinwr+kindr), vm+am)
endselect
  ! Loop on concurrent simulations
  do isim = 1, nsim	
    ic = nsim*(ideb-2+if)+isim

  !-----------------------------
  ! FULLY UPWIND
  if (sl(if) >= 0._krp) then
    flux%tabscal(1)%scal(ic) = vnl(if)*cell_l%density(if)             ! mass flux
    flux%tabscal(2)%scal(ic) = vnl(if)*(rel + cell_l%pressure(if))    ! energy flux
    flux%tabvect(1)%vect(ic) = (vnl(if)*cell_l%density(if))*cell_l%velocity(if) &
                                    + cell_l%pressure(if)*fn(if)             ! momentum flux
  !-----------------------------
  ! FULLY UPWIND
  elseif (sr(if) <= 0._krp) then
    flux%tabscal(1)%scal(ic) = vnr(if)*cell_r%density(if)             ! mass flux
    flux%tabscal(2)%scal(ic) = vnr(if)*(rer + cell_r%pressure(if))    ! energy flux
    flux%tabvect(1)%vect(ic) = (vnr(if)*cell_r%density(if))*cell_r%velocity(if) &
                                    + cell_r%pressure(if)*fn(if)             ! momentum flux
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
      rvst = iks*( (rqL*cell_l%velocity(if)) + (pst-cell_l%pressure(if))*fn(if) )
      rest = iks*(rel*(sl(if)-vnl(if)) - cell_l%pressure(if)*vnl(if) + pst*Sst)
    else
      iks  = 1._krp/(sr(if) - Sst)
      rst  = rqR*iks
      pst  = cell_r%pressure(if) - rqR*(vnr(if)-Sst)
      rvst = iks*( (rqR*cell_r%velocity(if)) + (pst-cell_r%pressure(if))*fn(if) )
      rest = iks*(rer*(sr(if)-vnr(if)) - cell_r%pressure(if)*vnr(if) + pst*Sst)
    endif
  
  ! mass flux
    flux%tabscal(1)%scal(ic) = Sst*rst
  ! energy flux
    flux%tabscal(2)%scal(ic) = Sst*(rest + pst)
  ! momentum flux
    flux%tabvect(1)%vect(ic) = (Sst*rvst) + (pst*fn(if))
  endif
  
enddo ! loop on simulations

  

enddo ! loop on nflux
call delete(roe)

!--------------------------------------------------------------
! Jacobian calculation
!--------------------------------------------------------------
if (calc_jac) then

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with HLLC flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_hll)
    sl(1:nflux) = min(sl(1:nflux), 0._krp)
    sr(1:nflux) = max(sr(1:nflux), 0._krp)
    call calc_jac_hll(defsolver, defspat, nflux, face,          &
                      cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
  case(jac_hlldiag)
    sl(1:nflux) = min(sl(1:nflux), 0._krp)
    sr(1:nflux) = max(sr(1:nflux), 0._krp)
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

endsubroutine calc_flux_hllc

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2005 : creation, HLLC flux
! Aug 2005 : call to jacobian matrices
! Sep 2012 : kinetic evaluations
! Feb  2013 : kinetic/beta evaluations for hllc and hlle
!------------------------------------------------------------------------------!

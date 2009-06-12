!------------------------------------------------------------------------------!
! Procedure : calc_flux_ausmm                   Authors : J. Gressier
!
! Function
!   Computation of AUSMM flux for Euler equations
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_ausmm(defsolver, defspat, nflux, face, &
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
real(krp), dimension(taille_buffer) &
                        :: al, ar, mp, mm, pp, pm, rHl, rHr, ml, mr, ms
real(krp)               :: g, gig1, iks

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
  ml(if) = (cell_l%velocity(if).scal.face(if)%normale)/al(if)    ! face normal velocity (left  state)
  mr(if) = (cell_r%velocity(if).scal.face(if)%normale)/ar(if)    !                      (right state)
enddo

! -- Mach numbers functions and Pressure upwinding functions (van Leer basic ones) --

where (ml(1:nflux) >= 1._krp)
  mp(1:nflux) = ml(1:nflux)
  pp(1:nflux) = 1._krp
elsewhere (ml(1:nflux) <= -1._krp)
  mp(1:nflux) = 0._krp
  pp(1:nflux) = 0._krp
elsewhere ((ml(1:nflux) > -1._krp).and.(ml(1:nflux) < 1._krp))
  mp(1:nflux) = .25_krp*(ml(1:nflux)+1._krp)**2
  pp(1:nflux) = mp(1:nflux)*(2._krp-ml(1:nflux))
endwhere

where (mr(1:nflux) <= -1._krp)
  mm(1:nflux) = mr(1:nflux)
  pm(1:nflux) = 1._krp
elsewhere (mr(1:nflux) >= 1._krp)
  mm(1:nflux) = 0._krp
  pm(1:nflux) = 0._krp
elsewhere ((mr(1:nflux) > -1._krp).and.(mr(1:nflux) < 1._krp))
  mm(1:nflux) = -.25_krp*(mr(1:nflux)-1._krp)**2
  pm(1:nflux) = -mm(1:nflux)*(2._krp+mr(1:nflux))
endwhere

! -- AUSM Mach upwinding --

ms(1:nflux) = mp(1:nflux) + mm(1:nflux)
ml(1:nflux) = al(1:nflux) * max(0._krp, ms(1:nflux))
mr(1:nflux) = ar(1:nflux) * min(0._krp, ms(1:nflux))

! -- volumic total enthalpy (left and right) --

rHl(1:nflux) = gig1*cell_l%pressure(1:nflux) + .5_krp*cell_l%density(1:nflux)*sqrabs(cell_l%velocity(1:nflux))
rHr(1:nflux) = gig1*cell_r%pressure(1:nflux) + .5_krp*cell_r%density(1:nflux)*sqrabs(cell_r%velocity(1:nflux))

do if = 1, nflux

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = ml(if)*cell_l%density(if) + mr(if)*cell_r%density(if)
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = ml(if)*rHl(if) + mr(if)*rHr(if)
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = (ml(if)*cell_l%density(if))*cell_l%velocity(if) &
                                  + (mr(if)*cell_r%density(if))*cell_r%velocity(if) &
                                  + (pp(if)*cell_l%pressure(if) + pm(if)*cell_r%pressure(if))*face(if)%normale
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
  case default
    call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif


endsubroutine calc_flux_ausmm

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2004 : creation, AUSMM flux
! Aug 2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

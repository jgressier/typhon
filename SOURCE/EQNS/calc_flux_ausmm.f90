!------------------------------------------------------------------------------!
! Procedure : calc_flux_ausmm                  Authors : J. Gressier
!                                              Created : September 2005
! Fonction
!   Computation of AUSMM flux for Euler equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_ausmm(defsolver, defspat, nflux, face,        &
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
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! jac associees

! -- Internal variables --
integer                 :: if
real(krp), dimension(taille_buffer) &
                        :: al, ar, mp, mm, pp, pm, rHl, rHr, ml, mr, ms
real(krp)               :: g, gig1, iks

! -- BODY --

g    = defsolver%defns%properties(1)%gamma
gig1 = g/(g - 1._krp)

!-------------------------------
! Flux computation

! -- speed of sound --

do if = 1, nflux
  al(if) = sqrt(g*cell_l(if)%pressure/cell_l(if)%density)        ! sound speed          (left state)
  ar(if) = sqrt(g*cell_r(if)%pressure/cell_r(if)%density)        !                      (right state)
enddo

! -- Mach numbers -- 

do if = 1, nflux
  ml(if) = (cell_l(if)%velocity.scal.face(if)%normale)/al(if)    ! face normal velocity (left  state)
  mr(if) = (cell_r(if)%velocity.scal.face(if)%normale)/ar(if)    !                      (right state)
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

do if = 1, nflux
  rHl(if) = gig1*cell_l(if)%pressure + .5_krp*cell_l(if)%density*sqrabs(cell_l(if)%velocity)
  rHr(if) = gig1*cell_r(if)%pressure + .5_krp*cell_r(if)%density*sqrabs(cell_r(if)%velocity)
enddo


do if = 1, nflux

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = ml(if)*cell_l(if)%density + mr(if)*cell_r(if)%density
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = ml(if)*rHl(if) + mr(if)*rHr(if)
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = (ml(if)*cell_l(if)%density)*cell_l(if)%velocity &
                                  + (mr(if)*cell_r(if)%density)*cell_r(if)%velocity &
                                  + (pp(if)*cell_l(if)%pressure + pm(if)*cell_r(if)%pressure)*face(if)%normale
enddo

!--------------------------------------------------------------
! Calcul des jacobiennes
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
! July 2004 : creation, AUSMM flux
! Aug  2005 : call to jacobian matrices
!------------------------------------------------------------------------------!

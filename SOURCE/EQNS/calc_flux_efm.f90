!------------------------------------------------------------------------------!
! Procedure : calc_flux_efm                        Authors : Praveen. C
!                                                  Created : 27 July 2008
! Fonction
!   Computation of Kinetic flux for Euler equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_efm (defsolver, defspat, nflux, face,  &
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

! -- INPUTS --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat)       :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- OUTPUTS --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal variables --
integer                   :: if
type(v3d)                 :: fn, rvst
real(krp), dimension(taille_buffer) :: sl, sr, vnl, vnr
real(krp)                 :: g, ig1, iks
real(krp)                 :: am, al, ar, vm, rel, rer, rqL, rqR
real(krp)                 :: Sst, rst, pst, rest
real(krp)                 :: ml, mr, erfl, erfr, expl, expr, f1, f2, PI

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)
PI  = 4.0_krp*atan(1.0_krp)
f1  = sqrt(0.5_krp*g)
f2  = 1.0_krp/sqrt(2.0_krp*PI*g)

! -- Calcul du flux --

do if = 1, nflux

  fn      = face(if)%normale 
  vnl(if) = cell_l%velocity(if).scal.fn                ! face normal velocity (left  state)
  vnr(if) = cell_r%velocity(if).scal.fn                !                      (right state)
  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! sound speed          (left state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)
  ml  = f1*vnl(if)/al
  mr  = f1*vnr(if)/ar

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  ! error function
  erfl= 0.5_krp*(1.0_krp + erf(ml))
  erfr= 0.5_krp*(1.0_krp - erf(mr))

  ! exponential function
  expl= f2*al*exp(-ml*ml)
  expr= f2*ar*exp(-mr*mr)

  !-----------------------------
  ! Centered flux
  flux%tabscal(1)%scal(ideb-1+if) = cell_l%density(if) * ( vnl(if) * erfl + expl) + &
                                    cell_r%density(if) * ( vnr(if) * erfr - expr)
  flux%tabscal(2)%scal(ideb-1+if) = (rel + cell_l%pressure(if)) * vnl(if) * erfl + &
                                    (rer + cell_r%pressure(if)) * vnr(if) * erfr + &
                                    (rel + 0.5_krp*cell_l%pressure(if)) * expl - &
                                    (rer + 0.5_krp*cell_r%pressure(if)) * expr
  flux%tabvect(1)%vect(ideb-1+if) = (cell_l%pressure(if) * erfl  + &
                                     cell_r%pressure(if) * erfr ) * fn + &
                                     (vnl(if) * erfl + expl) * cell_l%density(if) * cell_l%velocity(if) + &
                                     (vnr(if) * erfr - expr) * cell_r%density(if) * cell_r%velocity(if)
enddo

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then

  !sl(1:nflux) = min(sl(1:nflux), 0._krp)
  !sr(1:nflux) = max(sr(1:nflux), 0._krp)

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with EFM flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_hll,jac_hlldiag)
    call erreur("Development", "HLL jacobian matrices not available with EFM flux")
  case default
    call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif

endsubroutine calc_flux_efm

!------------------------------------------------------------------------------!
! Changes history
!
! July 2008 : creation, Kinetic flux
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calc_flux_rusanov              Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Computation of RUSANOV flux for Euler equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_rusanov(defsolver, defspat, nflux, face,        &
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
type(st_nsetat)       :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal variables --
integer                     :: if
type(v3d), dimension(taille_buffer) :: fn
real(krp), dimension(taille_buffer) :: ray, vnl, vnr
real(krp)                   :: g, ig1, al, ar, rel, rer

! -- BODY --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Calculs preliminaires --


! -- Calcul du flux --

do if = 1, nflux
  fn(if)  = face(if)%normale 
enddo

vnl(1:nflux) = cell_l%velocity(1:nflux).scal.fn(1:nflux)       ! face normal velocity (left  state)
vnr(1:nflux) = cell_r%velocity(1:nflux).scal.fn(1:nflux)       !                      (right state)

do if = 1, nflux

  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! sound speed          (left state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)
  ray(if) = max(abs(vnl(if))+al, abs(vnr(if))+ar )

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = .5_krp*( (vnl(if)+ray(if))*cell_l%density(if) + (vnr(if)-ray(if))*cell_r%density(if) )
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = .5_krp*( (vnl(if)+ray(if))*rel + (vnr(if)-ray(if))*rer &
                                  + vnl(if)*cell_l%pressure(if) + vnr(if)*cell_r%pressure(if) )
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = .5_krp*( ((vnl(if)+ray(if))*cell_l%density(if))*cell_l%velocity(if) &
                                  + ((vnr(if)-ray(if))*cell_r%density(if))*cell_r%velocity(if) &
                                  + (cell_l%pressure(if) + cell_r%pressure(if))*fn(if) )
enddo

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with RUSANOV flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_rusanov)
    call calc_jac_rusanov(defsolver, defspat, nflux, face,          &
                      cell_l, cell_r, ray, vnl, vnr, ideb, jacL, jacR)
  case(jac_hll, jac_hlldiag)
    call erreur("Development", "HLL jacobian matrices not available with RUSANOV flux")

  case default
    call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif


endsubroutine calc_flux_rusanov

!------------------------------------------------------------------------------!
! Changes history04
!
! Apr  2008 : creation, RUSANOV flux
!------------------------------------------------------------------------------!

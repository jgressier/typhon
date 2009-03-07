!------------------------------------------------------------------------------!
! Procedure : calc_flux_inviscid                  Authors : J. Gressier
!                                                Created : February 2004
! Fonction                                       Modif   : (cf history)
!   Computation of INVISCID flux for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_flux_inviscid(defsolver, defspat, nflux, ideb, face, &
                             cell_l, cell_r, flux,    &
                             calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use MATRIX_ARRAY

implicit none

! -- INPUTS --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer, intent(in)   :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face)         :: face(nflux)      ! geom. data of faces
type(st_genericfield) :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! should compute jacobian matrices or not


! -- OUTPUTS --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! jac associees

! -- Internal variables --
type(st_nsetat)       :: QL, QR
integer               :: ifin
integer               :: icell(nflux)

! -- BODY --

ifin = ideb+nflux-1

! pointers links
QL%density  => cell_l%tabscal(1)%scal(ideb:ifin)
QR%density  => cell_r%tabscal(1)%scal(ideb:ifin)
QL%pressure => cell_l%tabscal(2)%scal(ideb:ifin)
QR%pressure => cell_r%tabscal(2)%scal(ideb:ifin)
QL%velocity => cell_l%tabvect(1)%vect(ideb:ifin)
QR%velocity => cell_r%tabvect(1)%vect(ideb:ifin)

!----------------------------------------------------------------------
! computation of INVISCID fluxes
!----------------------------------------------------------------------


select case(defspat%sch_hyp)
case(sch_ausmm)
  call calc_flux_ausmm(defsolver, defspat, nflux, face,        &
                       QL, QR, flux, ideb,                     &
                       calc_jac, jacL, jacR)
case(sch_rusanov)
  call calc_flux_rusanov(defsolver, defspat, nflux, face,      &
                      QL, QR, flux, ideb,                      &
                      calc_jac, jacL, jacR)
case(sch_hlle)
  call calc_flux_hlle(defsolver, defspat, nflux, face,         &
                      QL, QR, flux, ideb,                      &
                      calc_jac, jacL, jacR)
case(sch_hllc)
  call calc_flux_hllc(defsolver, defspat, nflux, face,         &
                      QL, QR, flux, ideb,                      &
                      calc_jac, jacL, jacR)
case(sch_efm)
  call calc_flux_efm(defsolver, defspat, nflux, face,         &
                     QL, QR, flux, ideb,                      &
                     calc_jac, jacL, jacR)
case default
  call erreur("error","numerical scheme not implemented (flux computation)")
endselect


endsubroutine calc_flux_inviscid

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2007 : creation, INVISCID fluxes
!------------------------------------------------------------------------------!

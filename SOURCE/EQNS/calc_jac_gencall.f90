!------------------------------------------------------------------------------!
! Procedure : calc_jac_gencall                      Authors : J. Gressier
!
! Function
!   Generic call to jacobian matrices
!
!------------------------------------------------------------------------------!
subroutine calc_jac_gencall(defsolver, defspat, nflux, face,        &
                        cell_l, cell_r, mnl, mnr, al, ar, ideb, jacL, jacR)
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
integer               :: ideb             ! index of first flux (offset)
type(st_face), dimension(1:nflux) &
                      :: face             ! geom. data of faces
type(st_nsetat)             :: cell_l, cell_r   ! primitive variables array
real(krp), dimension(nflux) :: mnl, mnr, al, ar

! -- Inputs/Outputs --

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal parameters --

! -- BODY --

select case(defspat%jac_hyp)
case(jac_vlh) 
  call calc_jac_vlh(defsolver, defspat, nflux, face,        &
                    cell_l, cell_r, mnl, mnr, al, ar, ideb, jacL, jacR)
case default
  call erreur("Internal error", "unknown jacobian expression for Euler hyperbolic fluxes")
endselect


endsubroutine calc_jac_gencall

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2010 : creation
!------------------------------------------------------------------------------!

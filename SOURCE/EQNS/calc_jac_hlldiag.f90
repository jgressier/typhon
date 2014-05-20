!------------------------------------------------------------------------------!
! Procedure : calc_jac_hlldiag                  Authors : J. Gressier
!
!> @brief Computes HLL Jacobian matrices of a numerical flux
!>   (diagonal simplified expression)
!------------------------------------------------------------------------------!
subroutine calc_jac_hlldiag(defsolver, defspat, nflux, fn,        &
                        cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
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
type(v3d)             :: fn(nflux)        ! face normals
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
real(krp), dimension(nflux) &
                      :: sl, sr, vnl, vnr

! -- Inputs/Outputs --

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal parameters --
integer(kip) :: i

! -- Body --

jacL%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp
jacR%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp
do i = 1, 5
  jacL%mat(i, i, ideb:ideb-1+nflux) = sr(1:nflux)
  jacR%mat(i, i, ideb:ideb-1+nflux) = sl(1:nflux)
enddo


endsubroutine calc_jac_hlldiag

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2005 : creation
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calc_jac_hll                      Authors : J. Gressier
!
!> @brief Computes HLL Jacobian matrices of a numerical flux
!------------------------------------------------------------------------------!
subroutine calc_jac_hll(defsolver, defspat, nflux, fn,        &
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
type(v3d)             :: fn(nflux)        ! geom. data of faces
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
real(krp), dimension(nflux) &
                      :: sl, sr, vnl, vnr

! -- Inputs/Outputs --

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal parameters --
integer(kip) :: i

! -- Body --

call calc_jacflux(defsolver%defns, nflux, fn, cell_l, vnl, ideb, jacL)
call calc_jacflux(defsolver%defns, nflux, fn, cell_r, vnr, ideb, jacR)

do i = 1, 5
  jacL%mat(i, i, ideb:ideb-1+nflux) = jacL%mat(i, i, ideb:ideb-1+nflux) - sl(1:nflux)
  jacR%mat(i, i, ideb:ideb-1+nflux) = jacR%mat(i, i, ideb:ideb-1+nflux) - sr(1:nflux)
enddo

do i = 1, nflux
  jacL%mat(1:5, 1:5, ideb-1+i) = ( sr(i)/(sr(i)-sl(i)))*jacL%mat(1:5, 1:5, ideb-1+i)
  jacR%mat(1:5, 1:5, ideb-1+i) = (-sl(i)/(sr(i)-sl(i)))*jacR%mat(1:5, 1:5, ideb-1+i)
enddo


endsubroutine calc_jac_hll

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2005 : creation
!------------------------------------------------------------------------------!

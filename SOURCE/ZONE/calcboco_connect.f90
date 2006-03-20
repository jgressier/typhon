!------------------------------------------------------------------------------!
! Procedure : calcboco_connect                         Authors : J. Gressier
!                                                      Created : October 2005
! Fonction 
!   Computation & exchange of boundary conditions for connection conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect(defsolver, grid, defspat, boco)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use GRID_CONNECT
use DEFFIELD

implicit none

! -- Inputs --
type(mnu_solver)       :: defsolver        ! type d'equation a resoudre
type(mnu_spat)         :: defspat
type(st_ustboco)       :: boco

! -- Outputs --
type(st_grid)          :: grid             ! maillage en entree, champ en sortie

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! -- BODY --

idef = boco%idefboco

select case(boco%gridcon%contype)

case(gdcon_match)

  call calcboco_connect_match(defsolver, grid%umesh, grid%field%etatprim, boco)

case(gdcon_nomatch)
  call erreur("Development","non matching connection not implemented")
case(gdcon_coarse_fine)
  call erreur("Development","coarse/fine connection not implemented")
case(gdcon_fine_coarse)
  call erreur("Development","fine/coarse connection not implemented")

case default
  call erreur("Internal error","unknown connection parameter")

endselect


endsubroutine calcboco_connect

!------------------------------------------------------------------------------!
! Changes history
!
! Oct   2005 : Created
!------------------------------------------------------------------------------!

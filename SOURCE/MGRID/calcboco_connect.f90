!------------------------------------------------------------------------------!
! Procedure : calcboco_connect                         Authors : J. Gressier
!                                                      Created : October 2005
! Fonction 
!   Computation & exchange of boundary conditions for connection conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect(defsolver, defspat, grid)

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

! -- Outputs --
type(st_grid)          :: grid             ! maillage en entree, champ en sortie

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! ----------------------------------- BODY -----------------------------------

! only compute connections (idef <= 0)

do ib = 1, grid%umesh%nboco

  idef = grid%umesh%boco(ib)%idefboco
  if (idef <= 0) then

    select case(grid%umesh%boco(ib)%gridcon%contype)

    case(gdcon_match)

      ! send and receive data of this grid (from other grids)
      ! 
      call calcboco_connect_match(defsolver, grid%umesh, grid%field%etatprim, grid%umesh%boco(ib))

    case(gdcon_nomatch)
      call erreur("Development","non matching connection not implemented")

    case(gdcon_per_match)

      ! compute periodic matching conditions (only available for periodic def. inside a grid)
      ! 
      call calcboco_connect_per_match(defsolver, grid%umesh, grid%field%etatprim, grid%umesh%boco(ib))

    case(gdcon_per_nomatch)
      call erreur("Development","periodic non matching connection not implemented")

    case(gdcon_coarse_fine)
      call erreur("Development","coarse/fine connection not implemented")

    case(gdcon_fine_coarse)
      call erreur("Development","fine/coarse connection not implemented")

    case default
      call erreur("Internal error","unknown connection parameter")

    endselect

  endif
enddo

endsubroutine calcboco_connect

!------------------------------------------------------------------------------!
! Changes history
!
! Oct   2005 : Created
!------------------------------------------------------------------------------!

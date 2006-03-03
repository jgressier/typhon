!------------------------------------------------------------------------------!
! MODULE : GRID_CONNECT                              Authors : J. Gressier
!                                                    Created : Oct 2005
! Fonction
!   Structures definition for connection of cells between grids
!
!------------------------------------------------------------------------------!

module GRID_CONNECT

use TYPHMAKE      ! machine accuracy

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: gdcon_match        = 01
integer(kpp), parameter :: gdcon_nomatch      = 10
integer(kpp), parameter :: gdcon_coarse_fine  = 20
integer(kpp), parameter :: gdcon_fine_coarse  = 21


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! GRIDCONNECT definition
!------------------------------------------------------------------------------!

type st_gridconnect
  integer(kip)          :: zone_id
  integer(kip)          :: grid_id
  integer(kpp)          :: contype              ! connection type
  integer(kip), pointer :: i_param(:)           ! parameter of connection
  real(kpp),    pointer :: r_param(:)           ! parameter of connection
  integer(kip)          :: grid1,     grid2     ! grid id 
  integer(kip)          :: nc1,       nc2       ! number of cells connected
  integer(kip), pointer :: cells1(:), cells2(:) ! lists  of cells connected
  type(st_gridconnect), pointer &
                        :: next
endtype st_gridconnect


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_gridconnect
endinterface

interface delete
  module procedure delete_gridconnect
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Constructor : new GRID CONNECT
!------------------------------------------------------------------------------!
subroutine new_gridconnect(gridconnect)
implicit none
type(st_gridconnect)  :: gridconnect

  nullify(gridconnect%cells1)
  nullify(gridconnect%cells2)
  nullify(gridconnect%next)

endsubroutine new_gridconnect


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure GRIDCONNECT
!------------------------------------------------------------------------------!
subroutine delete_gridconnect(gridconnect)
implicit none
type(st_gridconnect)  :: gridconnect
integer           :: i     

  deallocate(gridconnect%cells1)
  deallocate(gridconnect%cells2)

endsubroutine delete_gridconnect



endmodule GRID_CONNECT

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!

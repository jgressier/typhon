!------------------------------------------------------------------------------!
! MODULE : GRID_CONNECT                              Authors : J. Gressier
!                                                    Created : Oct 2005
! Fonction
!   Structures definition for connection of cells between grids
!
!------------------------------------------------------------------------------!
module GRID_CONNECT

use TYPHMAKE      ! machine accuracy
use GENFIELD
use VEC3D

implicit none

! -- GLOBAL DEFINITION -------------------------------------------

integer(kpp), parameter :: igcon_send  = 11
integer(kpp), parameter :: igcon_recv  = 21

! -- contants for grid connection method --

integer(kpp), parameter :: gdcon_match        = 01
integer(kpp), parameter :: gdcon_nomatch      = 05
integer(kpp), parameter :: gdcon_per_match    = 11
integer(kpp), parameter :: gdcon_per_nomatch  = 15
integer(kpp), parameter :: gdcon_coarse_fine  = 20
integer(kpp), parameter :: gdcon_fine_coarse  = 21

! -- contants for data exchange method --

integer(kpp), parameter :: bccon_cell_state        = 10
integer(kpp), parameter :: bccon_cell_grad         = 15
integer(kpp), parameter :: bccon_face_state        = 20
integer(kpp), parameter :: bccon_face_grad         = 25

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition ST_BCCON : BOCO connectivity
!------------------------------------------------------------------------------!
type st_bccon
  integer(kpp)                   :: bccon_mode
  type(st_genericfield), pointer :: fsend, frecv ! pointer of send or receive fields
  integer(kip)                   :: nf
  integer(kip), pointer          :: isend(:), irecv(:)  ! index of data in fsend and frecv
endtype st_bccon

!------------------------------------------------------------------------------!
! GRIDCONNECT definition
!------------------------------------------------------------------------------!
type st_gridconnect
  integer(kpp)          :: contype              ! connection type
  integer(kip)          :: ilink                ! link index for external parameters
  real(krp)             :: rlink                ! link parameter
  integer(kip)          :: zone_id
  integer(kip)          :: grid_id
  integer(kip), pointer :: i_param(:)           ! parameter of connection
  real(kpp),    pointer :: r_param(:)           ! parameter of connection
  integer(kip)          :: grid1,     grid2     ! grid id 
  integer(kip)          :: nc1,       nc2       ! number of cells connected
  integer(kip), pointer :: cells1(:), cells2(:) ! lists  of cells connected
  integer(kip)          :: nsend, nrecv
  real(krp), pointer    :: rsend(:), rrecv(:)
  type(st_gridconnect), pointer &
                        :: next
endtype st_gridconnect


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_gridconnect
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Constructor : new GRID CONNECT
!------------------------------------------------------------------------------!
subroutine init_gridconnect(gridconnect)
implicit none
type(st_gridconnect)  :: gridconnect

  nullify(gridconnect%cells1)
  nullify(gridconnect%cells2)
  nullify(gridconnect%next)
  nullify(gridconnect%rsend)
  nullify(gridconnect%rrecv)

endsubroutine init_gridconnect


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure GRIDCONNECT
!------------------------------------------------------------------------------!
subroutine delete_gridconnect(gridconnect)
implicit none
type(st_gridconnect)  :: gridconnect
integer           :: i     

  if (associated(gridconnect%cells1)) deallocate(gridconnect%cells1)
  if (associated(gridconnect%cells2)) deallocate(gridconnect%cells2)
  if (associated(gridconnect%rsend))  deallocate(gridconnect%rsend)
  if (associated(gridconnect%rrecv))  deallocate(gridconnect%rrecv)

endsubroutine delete_gridconnect

!------------------------------------------------------------------------------!
! routine matching_index(c1, c2, i1, i2) 
!   computes index of matching nodes (matching defined by minimum distance)
!   inputs:   c1 = (A, B, C) and c2 = (B, C, A) 
!   returns:  i1 = (3, 1, 2) and i2 = (2, 3, 1)
!------------------------------------------------------------------------------!
subroutine matching_index(c1, c2, i1, i2)
implicit none
! -- INPUTS --
type(v3d), dimension(:), intent(in) :: c1, c2

! -- OUTPUTS --
integer, dimension(:), intent(out) :: i1, i2

! --- Private data ---
real(krp)   :: dist, mindist
integer     :: if1, if2, nf1, nf2, imin

nf1 = size(c1)
nf2 = size(c2)


do if1 = 1, nf1
  mindist = huge(mindist)
  imin    = 0
  do if2 = 1, nf2
    dist = abs( c1(if1) - c2(if2) )
    if (dist < mindist) then
      mindist = dist
      imin    = if2
    endif
  enddo
  i1(if1)  = imin
  i2(imin) = if1
enddo

endsubroutine matching_index


!------------------------------------------------------------------------------!
endmodule GRID_CONNECT

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created
! Aug  2008 : Add type of connection (mainly for matching periodicity)
! Aug  2008 : add routines to compute connectivity (extract from calc_connface)
!------------------------------------------------------------------------------!

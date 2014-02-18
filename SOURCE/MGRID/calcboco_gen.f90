!------------------------------------------------------------------------------!
! Procedure : calcboco_gen        
!                                 
! Fonction 
!   Computation & exchange of boundary conditions for
!   . connection conditions
!   . physical boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_gen(curtime, defsolver, defspat, grid, bccon_mode)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MGRID
use GRID_CONNECT
use DEFFIELD

implicit none

! -- Inputs --
real(krp)              :: curtime
type(mnu_solver)       :: defsolver 
type(mnu_spat)         :: defspat
integer(kpp)           :: bccon_mode       ! data exchange mode for connection

! -- Outputs --
type(st_grid)          :: grid             ! maillage en entree, champ en sortie

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord
type(st_bccon) :: bccon
! ----------------------------------- BODY -----------------------------------

bccon%bccon_mode = bccon_mode

select case(bccon_mode)
case(bccon_cell_state)
  bccon%fsend => grid%info%field_loc%etatprim
  bccon%frecv => grid%info%field_loc%etatprim
case(bccon_face_state)
  bccon%fsend => grid%info%field_loc%cell_l
  bccon%frecv => grid%info%field_loc%cell_r  
case(bccon_cell_grad)
  bccon%fsend => grid%info%field_loc%gradient
  bccon%frecv => grid%info%field_loc%gradient
case(bccon_face_grad)
  bccon%fsend => grid%info%field_loc%grad_l
  bccon%frecv => grid%info%field_loc%grad_r  
case default
  call error_stop("Internal error: unknown connection mode")
endselect

!------------------------------------------------------------------------------!
! send & receive matching and periodic connections

call calcboco_connect(defsolver, defspat, grid%umesh, bccon)

!------------------------------------------------------------------------------!
! physical BOCO

select case(bccon_mode)
case(bccon_cell_state, bccon_face_state)
  call calcboco_ust(curtime, defsolver, defspat, grid%umesh, bccon)
case(bccon_cell_grad, bccon_face_grad)
case default
  call error_stop("Internal error: unknown connection mode")
endselect

endsubroutine calcboco_gen
!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005: Created
! May  2009: integrate loop on boco for one grid dummmy argument
!------------------------------------------------------------------------------!

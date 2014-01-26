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
type(st_genericfield), pointer          :: fsend, frecv ! pointer of send or receive fields
integer(kip), dimension(:), allocatable :: isend, irecv ! index of data in fsent and frecv

! ----------------------------------- BODY -----------------------------------

select case(bccon_mode)
case(bccon_cell_state)
  fsend => grid%info%field_loc%etatprim
  frecv => grid%info%field_loc%etatprim
case(bccon_face_state)
  fsend => grid%info%field_loc%cell_l
  frecv => grid%info%field_loc%cell_r  
case(bccon_cell_grad)
  fsend => grid%info%field_loc%gradient
  frecv => grid%info%field_loc%gradient
case(bccon_face_grad)
  fsend => grid%info%field_loc%grad_l
  frecv => grid%info%field_loc%grad_r  
case default
  call error_stop("Internal error: unknown connection mode")
endselect

!------------------------------------------------------------------------------!
! send & receive matching and periodic connections

call calcboco_connect(defsolver, defspat, grid%umesh, bccon_mode, fsend, frecv)

!------------------------------------------------------------------------------!
! physical BOCO

!call calcboco_ust(curtime, defsolver, defspat, grid, bccon_mode, fsend, frecv)

endsubroutine calcboco_gen
!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005: Created
! May  2009: integrate loop on boco for one grid dummmy argument
!------------------------------------------------------------------------------!

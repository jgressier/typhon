!------------------------------------------------------------------------------!
! Procedure : calc_rhs 
! 
! Fonction                                       Modif  : (cf history)
!   Time Integration during one timestep of ONE LEVEL of UST grid TREE structure
!
!------------------------------------------------------------------------------!
subroutine calc_rhs(dt, info, defsolver, gridlist, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use MODINFO
use MENU_ZONECOUPLING
use MATRIX_ARRAY
use GRID_CONNECT

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling        ! nombre de couplages de la zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage
! retour des residus a travers le champ field de la structure zone

! -- Internal variables --
type(st_mattab)        :: jacL, jacR       ! tableaux de jacobiennes des flux
type(st_grid), pointer :: pgrid
real(krp)              :: curtime

! -------------------------------- BODY --------------------------------

! -- Preparation du calcul --

pgrid => gridlist%first
do while (associated(pgrid))
  call calc_varprim(defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

    
! -------------------------------------------------------------------------------
! BOUNDARY CONDITIONS OF EACH GRID OF ONE LEVEL

curtime = info%cycle_start + info%cycle_time

pgrid => gridlist%first

do while(associated(pgrid))

  call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_cell_state)
  call calcboco_ust(curtime, defsolver, defsolver%defspat, pgrid)

  pgrid => pgrid%next

enddo

    
! -------------------------------------------------------------------------------
! GRADIENTS OF EACH GRID OF ONE LEVEL (only if necessary)

if (defsolver%defspat%calc_grad) then
  pgrid => gridlist%first
  do while (associated(pgrid))
    call calc_gradient(defsolver, defsolver%defspat, pgrid,                 &
                       pgrid%info%field_loc%etatprim, pgrid%info%field_loc%gradient)
    call calc_gradient_limite(defsolver, pgrid%umesh, pgrid%info%field_loc%gradient)

    call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_cell_grad)
    pgrid => pgrid%next
  enddo
endif

! -------------------------------------------------------------------------------
! HIGH ORDER EXTRAPOLATION

if (defsolver%defspat%calc_hresQ) then
  pgrid => gridlist%first
  do while (associated(pgrid))
    call calc_hres_states(defsolver, defsolver%defspat, pgrid, pgrid%info%field_loc)
    pgrid => pgrid%next
  enddo
endif

! -------------------------------------------------------------------------------
! INTEGRATION OF EACH GRID OF ONE LEVEL

pgrid => gridlist%first
do while (associated(pgrid))

  call integration_grid(dt, info%time_model, defsolver, &
                        pgrid, coupling, ncoupling, jacL, jacR)

  ! -- implicit resolution --

  select case(defsolver%deftime%tps_meth)

  !case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)

  case(tps_impl, tps_dualt)

      call tstep_implicit(pgrid%dtloc, info%time_model, defsolver, &
                          pgrid%umesh, pgrid%info%field_loc, &
                          coupling, ncoupling, jacL, jacR)

  endselect

  pgrid => pgrid%next
enddo


!-----------------------------
endsubroutine calc_rhs

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2006: created from integzone_tstep_usttree
! Nov  2007: only compute RHS, updating is done by calling routine, changed name
! May  2009: change name: treelevel_explicit to calc_rhs
!------------------------------------------------------------------------------!

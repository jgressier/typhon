!------------------------------------------------------------------------------!
! Procedure : integ_treelevel                    Auteur : J. Gressier
!                                                Date   : March 2006
! Fonction                                       Modif  : (cf history)
!   Time Integration during one timestep of ONE LEVEL of UST grid TREE structure
!
!------------------------------------------------------------------------------!
subroutine integ_treelevel(dt, info, defsolver, gridlist, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use MODINFO
use MENU_ZONECOUPLING
use MESHALE

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
type(st_grid), pointer :: pgrid
real(krp)              :: curtime

! -- BODY --

!----------------------------------
! Initialization

curtime = info%cycle_start + info%cycle_time
pgrid => gridlist%first
do while (associated(pgrid))

  call alloc_res(pgrid%info%field_loc)    ! internal test of allocation
  if (defsolver%defspat%calc_cellgrad) call alloc_cellgrad(pgrid%info%field_loc)  
  if (defsolver%defspat%calc_facegrad) call alloc_facegrad(pgrid%info%field_loc)

  select case(pgrid%info%gridtype)
  case(grid_ust)
    call ale_meshupdate(pgrid%umesh, defsolver, pgrid%optmem%gradcond_computed, curtime, dt) !1:zone%defsolver%nboco
  case(grid_str)
    ! nothing to do
  case default
    call error_stop("Development: gridtype="//trim(strof(pgrid%info%gridtype))// &
                    "unknown type of grid (integ_treelevel)")
  endselect

  pgrid => pgrid%next
enddo

!----------------------------------
! Time integration

select case(defsolver%deftime%tps_meth)

case(tps_expl, tps_impl)
  call treelevel_explicit(dt, info, defsolver, gridlist, coupling, ncoupling)

case(tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4, tps_lsrk25bb, tps_lsrk26bb, tps_lsrk12bs, tps_lsrk13bs)
  call treelevel_rungekutta(dt, info, defsolver, gridlist, coupling, ncoupling)

case(tps_dualt)
  call error_stop("Development: DUAL TIME method  not yet implemented (integ_treelevel)")

case default
  call error_stop("Development: unknown integration method (integ_treelevel)")
endselect

! -- update main field of each grid with (last) RHS --

call update_field(info, defsolver, gridlist)


!-----------------------------
endsubroutine integ_treelevel

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2006: created from integzone_tstep_usttree
! Nov  2007: choice of time integration method
!------------------------------------------------------------------------------!

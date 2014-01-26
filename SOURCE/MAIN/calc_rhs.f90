!------------------------------------------------------------------------------!
! Procedure : calc_rhs                          Authors : J. Gressier
! 
! Function
!   Time Integration during one timestep of ONE LEVEL of UST grid TREE structure
!
! Defaults/Limitations/Misc :
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
use SPARSE_MAT

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling       ! number of couplings of the zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! coupling data

! residuals are stored in gridlist%(pgrid)%info%field_loc%residu
! ?? retour des residus a travers le champ field de la structure zone

! -- Internal variables --
type(st_grid), pointer :: pgrid
real(krp)              :: curtime
type(st_spmat)         :: mat

! -- Body --

! -- Pre-processing --

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

if (defsolver%defspat%gradmeth /= gradnone) then

pgrid => gridlist%first
do while (associated(pgrid))

  select case(defsolver%defspat%gradmeth)
  !case(gradnone)
  !  ! nothing to do
  case(cellgrad_compactgauss)
    call calc_gradient_gauss(defsolver, defsolver%defspat, pgrid,                 &
                       pgrid%info%field_loc%etatprim, pgrid%info%field_loc%gradient)
    ! !!! DEV 
    ! for viscous fluxes or MUSCL extrapolation at BOCO, must be specific to BOCO physical type
    call calc_gradient_limite(defsolver, pgrid%umesh, pgrid%info%field_loc%gradient)
    call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_cell_grad)

  case(cellgrad_lsq, cellgrad_lsqw)
    call calc_gradient(defsolver, defsolver%defspat, pgrid,                 &
                       pgrid%info%field_loc%etatprim, pgrid%info%field_loc%gradient)
    ! !!! DEV 
    ! for viscous fluxes or MUSCL extrapolation at BOCO, must be specific to BOCO physical type
    call calc_gradient_limite(defsolver, pgrid%umesh, pgrid%info%field_loc%gradient)
    call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_cell_grad)

  case(facegrad_svm)
    call calc_gradface_svm(defsolver%defspat, pgrid%umesh, pgrid%info%field_loc%etatprim, &
                           pgrid%info%field_loc%grad_l, pgrid%info%field_loc%grad_r)
    call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_face_grad)

  case default
    call error_stop("Internal error: unknown GRADIENT computation method (calc_rhs)")
  endselect

  pgrid => pgrid%next
enddo

endif

! -------------------------------------------------------------------------------
! HIGH ORDER EXTRAPOLATION

if (defsolver%defspat%calc_hresQ) then

  pgrid => gridlist%first
  do while (associated(pgrid))
    call calc_hres_states(defsolver, defsolver%defspat, pgrid, pgrid%info%field_loc)
    !if (defsolver%defspat%method == hres_svm) then
    call calcboco_connect(defsolver, defsolver%defspat, pgrid, bccon_face_state)
    !call calcboco_ust(curtime, defsolver, defsolver%defspat, pgrid)
    !endif
    pgrid => pgrid%next
  enddo

endif

! -------------------------------------------------------------------------------
! INTEGRATION OF EACH GRID OF ONE LEVEL

pgrid => gridlist%first
do while (associated(pgrid))
  if (defsolver%deftime%implicite%calc_jacobian) &
      call init_implicit(pgrid%dtloc, defsolver, pgrid%umesh, mat)

  call integration_grid(dt, info%time_model, defsolver, &
                        pgrid, coupling, ncoupling, mat, curtime)

  ! -- implicit resolution --

  select case(defsolver%deftime%tps_meth)

  !case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)

  case(tps_impl, tps_dualt)

    if (defsolver%deftime%implicite%storage /= mat_none) then
      call tstep_implicit(pgrid%dtloc, info%time_model, defsolver, &
                          pgrid%umesh, pgrid%info%field_loc, &
                          coupling, ncoupling, mat)
      !call delete_spmat(mat) ! done into tstep_implicit
    endif

  endselect

  pgrid => pgrid%next
enddo

endsubroutine calc_rhs

!------------------------------------------------------------------------------!
! Changes history
!
! Mar 2006 : created from integzone_tstep_usttree
! Nov 2007 : only compute RHS, updating is done by calling routine, changed name
! May 2009 : change name: treelevel_explicit to calc_rhs
!------------------------------------------------------------------------------!

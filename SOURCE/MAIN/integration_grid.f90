!------------------------------------------------------------------------------!
! Procedure : integration_grid                  Authors : J. Gressier
!
! Function
!   Time step Integration of one grid : select integration method
!
!------------------------------------------------------------------------------!
subroutine integration_grid(dt, typtemps, defsolver, grid, &
                            coupling, ncoupling, mat, curtime)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_GEN
use MGRID
use MENU_ZONECOUPLING
use MATRIX_ARRAY
use SPARSE_MAT

implicit none

! -- Inputs --
real(krp)        :: dt               ! CFL time step
character        :: typtemps         ! time model (STEADY, UNSTEADY, PERIODIC)
type(mnu_solver) :: defsolver        ! solver parameters
integer          :: ncoupling        ! number of couplings of the zone
real(krp)        :: curtime          ! temps courant

! -- Inputs/Outputs --
type(st_grid)    :: grid             ! grid
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling         ! coupling data

! -- Outputs --
type(st_spmat)   :: mat

! -- Internal variables --
type(st_mattab)       :: jacL, jacR  ! flux jacobian matrices
type(st_genericfield) :: flux        ! physical flux

logical :: calc_jac

! -- Body --


! -- jacobians allocation --

select case(defsolver%deftime%tps_meth)
case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4, tps_lsrk25bb, tps_lsrk26bb, tps_lsrk12bs, tps_lsrk13bs)
  calc_jac = .false.
case(tps_impl, tps_dualt)
  calc_jac = .true.
  call new(jacL, grid%umesh%nface, defsolver%nequat)
  call new(jacR, grid%umesh%nface, defsolver%nequat)
case default
  call error_stop("Development: unknown integration method (integration_grid)")
endselect

! -- source terms and flux allocation (structure similar to field%etatcons) --

call new_genfield(flux, grid%umesh%nface, grid%info%field_loc%nscal, grid%info%field_loc%nvect, 0)
call init_genericfield(grid%info%field_loc%residu, 0._krp, v3d(0._krp, 0._krp, 0._krp))

! The whole mesh can be split here into fixed sized blocks
! for optimization of memory consumption and vectorization

select case(defsolver%typ_solver)
case(solKDIF)
  call integration_kdif_ust(defsolver, defsolver%defspat, grid%umesh, grid%info%field_loc, &
                            flux, calc_jac, jacL, jacR)
case(solNS)
  call integration_ns_ust(defsolver, defsolver%defspat, grid%umesh, grid%info%field_loc, &
                          flux, calc_jac, jacL, jacR, curtime)
case default
  call erreur("internal error (integration_grid)", "unknown or unexpected solver")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(grid%dtloc, grid%umesh, flux, grid%info%field_loc%residu, calc_jac, jacL, jacR)

call ale_correction(defsolver, grid%umesh, grid%info%field_loc)

!-----------------------------------------------------------------------
! BOCO HISTORY
!-----------------------------------------------------------------------

call integ_ustboco(grid%umesh, grid%info%field_loc, flux)

select case(defsolver%deftime%tps_meth)
case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)
!-----------------------------------------------------------------------
! COUPLING SPECIFIC actions
!-----------------------------------------------------------------------

  select case(typtemps)
    case(time_unsteady) ! corrections de flux seulement en instationnaire
    !
    ! Calcul de l'"energie" a l'interface, en vue de la correction de flux,
    ! pour le couplage avec echanges espaces
    !DVT : flux%tabscal(1) !
    if (ncoupling>0) then
      call accumulfluxcorr(grid%dtloc, defsolver, grid%umesh%nboco, grid%umesh%boco, &
                           grid%umesh%nface, flux%tabscal(1)%scal, ncoupling, &
                           coupling, grid%info%field_loc, grid%umesh, defsolver%defspat)
    endif
    !
  endselect

endselect

! -- source terms and flux deletion --

call delete(flux)

!--------------------------------------------------
! build implicit system
!--------------------------------------------------
select case(defsolver%deftime%tps_meth)

!case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)

case(tps_impl, tps_dualt)

  call build_implicit(grid%dtloc, defsolver%deftime, grid%umesh, jacL, jacR, mat, grid%info%field_loc%residu)

  call delete(jacL)
  call delete(jacR)

endselect

!------------------------------------------------------------------------------!
contains

subroutine ale_correction(defsolver, umesh, field)
implicit none

! -- INPUTS --
type(mnu_solver) :: defsolver        ! solver parameters
type(st_ustmesh) :: umesh            ! mesh properties

! -- INPUT/OUTPUT --
type(st_field)   :: field            ! champ de residus

! -- Private DATA --
integer               :: ic   ! index de cellules
integer               :: ip   ! index de variables
real(krp)             :: dv   ! volume difference

select case(defsolver%defale%type)
case(ale_none)   
case(ale_global, ale_body)
  !$OMP PARALLEL DO private(ic, ip, dv)
  do ic = 1, umesh%ncell_int
    dv = (defsolver%defale%old_volume(ic) - umesh%mesh%volume(ic,1,1))/umesh%mesh%volume(ic,1,1)
    do ip = 1, field%residu%nscal
      field%residu%tabscal(ip)%scal(ic) =  field%residu%tabscal(ip)%scal(ic) + dv*field%etatcons%tabscal(ip)%scal(ic)
    enddo
    do ip = 1, field%residu%nvect
      field%residu%tabvect(ip)%vect(ic) = field%residu%tabvect(ip)%vect(ic) + dv*field%etatcons%tabvect(ip)%vect(ic) 
    enddo
  enddo
  !$OMP END PARALLEL DO
case default
  call cfd_error("unknown ALE parameter (integration_grid)")
endselect

endsubroutine

endsubroutine integration_grid
!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2003 : creation
! Jul 2003 : ajout corrections de  flux
! Oct 2003 : corrections de flux seulement en instationnaire
! Apr 2004 : changement de nom  integration_ustdomaine -> integration_grid
!            appel des routines d'integration temporelle
! Oct 2004 : field chained list
! Sep 2005 : local time stepping
! Dec 2010 : init_genericfield(grid%info%field_loc%residu,...) moved from flux_to_res
!------------------------------------------------------------------------------!

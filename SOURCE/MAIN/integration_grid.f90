!------------------------------------------------------------------------------!
! Procedure : integration_grid
!
! Function
!   Time step Integration of one grid : select integration method
!
!------------------------------------------------------------------------------!
subroutine integration_grid(dt, typtemps, defsolver, grid, &
                            coupling, ncoupling, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_GEN
use MGRID
use MENU_ZONECOUPLING
use MATRIX_ARRAY

implicit none

! -- Inputs --
real(krp)        :: dt               ! pas de temps CFL
character        :: typtemps         ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Inputs/Outputs --
type(st_grid)    :: grid             ! domaine non structure a integrer
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! tableaux de jacobiennes des flux

! -- Internal variables --
type(st_genericfield) :: flux             ! tableaux des flux

logical :: calc_jac

! -- BODY --


! -- jacobians allocation --

select case(defsolver%deftime%tps_meth)
case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)
  calc_jac = .false.
case(tps_impl, tps_dualt)
  calc_jac = .true.
  call new(jacL, grid%umesh%nface, defsolver%nequat)
  call new(jacR, grid%umesh%nface, defsolver%nequat)
case default
  call erreur("Development","unknown integration method (integration_grid)")
endselect

! -- source terms and flux allocation (structure similar to field%etatcons) --

call new(flux, grid%umesh%nface, grid%info%field_loc%nscal, grid%info%field_loc%nvect, 0)

! The whole mesh can be split here into fixed sized blocks
! for optimization of memory consumption and vectorization

select case(defsolver%typ_solver)
case(solKDIF)
  call integration_kdif_ust(defsolver, defsolver%defspat, grid%umesh, grid%info%field_loc, &
                            flux, calc_jac, jacL, jacR)
case(solNS)
  call integration_ns_ust(defsolver, defsolver%defspat, grid%umesh, grid%info%field_loc, &
                          flux, calc_jac, jacL, jacR)
case default
  call erreur("internal error (integration_grid)", "unknown or unexpected solver")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(grid%dtloc, grid%umesh, flux, grid%info%field_loc%residu, calc_jac, jacL, jacR)

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


endsubroutine integration_grid
!------------------------------------------------------------------------------!
! changes history
!
! avr  2003 : creation de la procedure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
!             appel des routines d'integration temporelle
! oct  2004 : field chained list
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!

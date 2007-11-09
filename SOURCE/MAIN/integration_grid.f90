!------------------------------------------------------------------------------!
! Procedure : integration_grid
!
! Function
!   Time step Integration of one grid : select integration method
!
!------------------------------------------------------------------------------!
subroutine integration_grid(dt, typtemps, defsolver, grid, &
                            coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MGRID
use MENU_ZONECOUPLING

implicit none

! -- INPUTS --
real(krp)        :: dt               ! pas de temps CFL
character        :: typtemps         ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
integer          :: ncoupling        ! nombre de couplages de la zone

! -- INPUTS/OUTPUTS --
type(st_grid)    :: grid             ! domaine non structure a integrer
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage

! -- Internal variables --


! -- BODY --


select case(defsolver%deftime%tps_meth)

case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)
  call tstep_explicit(grid%dtloc, typtemps, defsolver, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case(tps_impl, tps_dualt)
  call tstep_implicit(grid%dtloc, typtemps, defsolver, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case default
  call erreur("Development","unknown integration method (integration_grid)")
endselect



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

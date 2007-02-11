!------------------------------------------------------------------------------!
! Procedure : integration_grid            Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf historique)
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
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

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
character        :: typtemps         ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(st_grid)    :: grid             ! domaine non structure a integrer
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage

! -- Internal variables --


! -- Body --


select case(defsolver%deftime%tps_meth)

case(tps_expl)
  call explicit_step(grid%dtloc, typtemps, defsolver, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case(tps_rk)
  call erreur("developpement","methode RUNGE KUTTA non implementee")

case(tps_impl)
  call implicit_step(grid%dtloc, typtemps, defsolver, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case(tps_dualt)
  call erreur("developpement","methode DUAL TIME non implementee")

case default
  call erreur("incoherence","parametre inattendu (integration_grid)")
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

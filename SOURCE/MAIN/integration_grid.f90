!------------------------------------------------------------------------------!
! Procedure : integration_grid            Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf historique)
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_grid(dt, typtemps, defsolver, defspat, deftime, grid, &
                            coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MGRID
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
character        :: typtemps         ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(mnu_spat)   :: defspat          ! paramètres d'intégration spatiale
type(mnu_time)   :: deftime          ! paramètres d'intégration spatiale
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Declaration des entrées/sorties --
type(st_grid)    :: grid             ! domaine non structuré à intégrer
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! données de couplage

! -- Declaration des variables internes --


! -- Debut de la procedure --


select case(deftime%tps_meth)

case(tps_expl)
  call explicit_step(dt, typtemps, defsolver, defspat, deftime, grid%umesh, grid%field_loc, &
                     coupling, ncoupling)

case(tps_rk)
  call erreur("developpement","méthode RUNGE KUTTA non implémentée")

case(tps_impl)
  call implicit_step(dt, typtemps, defsolver, defspat, deftime, grid%umesh, grid%field_loc, &
                     coupling, ncoupling)

case(tps_dualt)
  call erreur("developpement","méthode DUAL TIME non implémentée")

case default
  call erreur("incohérence","paramètre inattendu (integration_grid)")
endselect



endsubroutine integration_grid

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003 : création de la procédure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
!             appel des routines d'integration temporelle
! oct  2004 : field chained list
!------------------------------------------------------------------------------!

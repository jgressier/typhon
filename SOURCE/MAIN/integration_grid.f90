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

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
character        :: typtemps         ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(mnu_time)   :: deftime          ! parametres d'integration spatiale
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(st_grid)    :: grid             ! domaine non structure a integrer
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage

! -- Declaration des variables internes --


! -- Debut de la procedure --


select case(deftime%tps_meth)

case(tps_expl)
  call explicit_step(dt, typtemps, defsolver, defspat, deftime, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case(tps_rk)
  call erreur("developpement","methode RUNGE KUTTA non implementee")

case(tps_impl)
  call implicit_step(dt, typtemps, defsolver, defspat, deftime, grid%umesh, grid%info%field_loc, &
                     coupling, ncoupling)

case(tps_dualt)
  call erreur("developpement","methode DUAL TIME non implementee")

case default
  call erreur("incoherence","parametre inattendu (integration_grid)")
endselect



endsubroutine integration_grid

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003 : creation de la procedure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
!             appel des routines d'integration temporelle
! oct  2004 : field chained list
!------------------------------------------------------------------------------!

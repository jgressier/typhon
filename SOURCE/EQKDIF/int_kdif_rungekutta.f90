!------------------------------------------------------------------------------!
! Procedure : int_eqkdif_rungekutta       Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  :
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine int_eqkdif_rungekutta(dt, defsolver, domaine)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH

implicit none

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(st_ustmesh) :: domaine          ! domaine non structure a integrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --


! -- Debut de la procedure --

print*,"!!! DEBUG integration eqkdif"



endsubroutine int_eqkdif_rungekutta

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : integration_kdif_ust        Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  :
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_kdif_ust(dt, defsolver, domaine)

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

!print*,"!!! DEBUG integration kdif"

! A ce niveau, on est cense appeler une routine qui integre aussi bien les flux
! dans un domaine structure que dans un domaine non structure
! On peut ici decouper la maillage complet en blocs de taille fixe pour optimiser
! l'encombrement memoire et la vectorisation

call calc_kdif_flux(defsolver)


endsubroutine integration_kdif_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

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

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(st_ustmesh) :: domaine          ! domaine non structuré à intégrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --


! -- Debut de la procedure --

!print*,"!!! DEBUG integration kdif"

! A ce niveau, on est censé appeler une routine qui intègre aussi bien les flux
! dans un domaine structuré que dans un domaine non structuré
! On peut ici découper la maillage complet en blocs de taille fixé pour optimiser
! l'encombrement mémoire et la vectorisation

call calc_kdif_flux(defsolver)


endsubroutine integration_kdif_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : integration_strdomaine         Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  :
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_strdomaine(dt, defsolver, domaine)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use STRMESH

implicit none

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(st_block)   :: domaine          ! domaine structuré à intégrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --


! -- Debut de la procedure --

select case(defsolver%typ_solver)
case(solNS)
  !call int_eqns_rungekutta(dt, defsolver, domaine)
case default
  call erreur("incohérence interne (integration_strdomaine)", "solveur inconnu")
endselect


endsubroutine integration_strdomaine

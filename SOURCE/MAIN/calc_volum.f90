!------------------------------------------------------------------------------!
! Procedure : calc_volum                  Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  :
!   Calcul des termes sources volumiques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_volum(typ_solver, domaine)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use STRMESH

implicit none

! -- Declaration des entrées --
type(mnu_solver) :: typ_solver       ! type d'équation à résoudre
type(st_block)   :: domaine          ! domaine structuré à intégrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --


! -- Debut de la procedure --



endsubroutine calc_volum

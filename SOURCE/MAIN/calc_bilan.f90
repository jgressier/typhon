!------------------------------------------------------------------------------!
! Procedure : calc_bilan                  Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  :
!   Calcul des bilans a partir des variables, des termes volumiques
!   et des termes surfaciques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_bilan(typ_solver, domaine)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use STRMESH

implicit none

! -- Declaration des entrees --
type(mnu_solver) :: typ_solver       ! type d'equation a resoudre
type(st_block)   :: domaine          ! domaine structure a integrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --


! -- Debut de la procedure --



endsubroutine calc_bilan

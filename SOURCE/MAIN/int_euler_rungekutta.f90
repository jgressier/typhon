!------------------------------------------------------------------------------!
! Procedure : int_euler_rungekutta        Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  :
!   Integration d'un domaine : équations d'Euler par Runge Kutta
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine int_euler_rungekutta(dt, typ_solver, domaine)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use STRMESH

implicit none

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: typ_solver       ! type d'équation à résoudre
type(st_block)   :: domaine          ! domaine structuré à intégrer

! -- Declaration des sorties --
! domaine

! -- Declaration des variables internes --
integer :: irk       ! pas courant de Runge-Kutta

! -- Debut de la procedure --

do irk = 1, 

  !call calc_gradients()

  call calc_flux_euler()

  call calc_volum()

  call calc_bilan()

enddo ! Runge-Kutta

endsubroutine int_euler_runge_kutta

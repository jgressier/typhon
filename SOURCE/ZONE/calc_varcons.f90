!------------------------------------------------------------------------------!
! Procedure : calc_varcons                Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction
!   Calcul des variables conservatives a partir des variables primitives
!   Reroutage vers des procedures specifiques aux solveurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_varcons(def_solver, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD

implicit none

! -- Declaration des entrees --
type(mnu_solver) :: def_solver       ! definition des parametres du solveur

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ primitives->conservatives

! -- Declaration des variables internes --


! -- Debut de la procedure --


select case(def_solver%typ_solver)
case(solKDIF)
  call calc_varcons_kdif(def_solver%defkdif, field)
case(solNS)
  call calc_varcons_ns(def_solver%defns, field)
case default
  call erreur("Incoherence interne (calc_varcons)","type de solveur inconnu")
endselect 


!-----------------------------
endsubroutine calc_varcons

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 : creation de la procedure
! july 2004 : NS solver (call calv_varcons_ns)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calc_varcons                Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juin 2003 (cf historique)
!   Calcul des variables conservatives à partir des variables primitives
!   Reroutage vers des procédures spécifiques aux solveurs
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

! -- Declaration des entrées --
type(mnu_solver) :: def_solver       ! définition des paramètres du solveur

! -- Declaration des entrées/sorties --
type(st_field)   :: field            ! champ primitives->conservatives

! -- Declaration des variables internes --


! -- Debut de la procedure --


select case(def_solver%typ_solver)
case(solKDIF)
  call calc_varcons_kdif(def_solver%defkdif, field)
case default
  call erreur("Incohérence interne (calc_varcons)","type de solveur inconnu")
endselect 


!-----------------------------
endsubroutine calc_varcons

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

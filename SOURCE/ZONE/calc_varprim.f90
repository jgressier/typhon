!------------------------------------------------------------------------------!
! Procedure : calc_varprim                Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juin 2003 (cf historique)
!   Calcul des variables primitives à partir des variables conservatives
!   Reroutage vers des procédures spécifiques aux solveurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_varprim(def_solver, field)

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
case(solNS)
  call calc_varprim_ns(def_solver%defns, field)
case(solKDIF)
  call calc_varprim_kdif(def_solver%defkdif, field)
case(solVORTEX)
  ! rien à faire
case default
  call erreur("Incohérence interne (calc_varprim)","type de solveur inconnu")
endselect 


!-----------------------------
endsubroutine calc_varprim

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 : création de la procédure
! july 2004 : NS solver (calc_varprim_ns)
!------------------------------------------------------------------------------!

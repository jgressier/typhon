!------------------------------------------------------------------------------!
! Procedure : calc_varprim                Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juin 2003 (cf historique)
!   Calcul des variables primitives a partir des variables conservatives
!   Reroutage vers des procedures specifiques aux solveurs
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

! -- Declaration des entrees --
type(mnu_solver) :: def_solver       ! definition des parametres du solveur

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ primitives->conservatives

! -- Declaration des variables internes --


! -- Debut de la procedure --


select case(def_solver%typ_solver)
case(solNS)
  call calc_varprim_ns(def_solver%defns, field)
case(solKDIF)
  call calc_varprim_kdif(def_solver%defkdif, field)
case(solVORTEX)
  ! rien a faire
case default
  call erreur("Incoherence interne (calc_varprim)","type de solveur inconnu")
endselect 


!-----------------------------
endsubroutine calc_varprim

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 : creation de la procedure
! july 2004 : NS solver (calc_varprim_ns)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : corr_varprim                Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  :
!   Calcul des variables primitives aux frontières de couplage,
!   tenant compte des corrections de flux. Orientation selon le solver.
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine corr_varprim(field, domaine, def_solver, dif_enflux, nb)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use MENU_SOLVER
use DEFFIELD
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrées --
type(st_ustmesh)      :: domaine          ! domaine non structuré à intégrer
type(mnu_solver)      :: def_solver     ! propriétés du solver
type(st_genericfield) :: dif_enflux       ! énergie à ajouter, pour correction de flux
integer               :: nb               ! index de la condition aux limites

! -- Declaration des entrées/sorties --
type(st_field)   :: field            ! champ des valeurs et résidus

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(def_solver%typ_solver)
case(solKDIF)
  call corr_varprim_kdif(field, domaine, def_solver%defkdif, dif_enflux, nb)
case default
  call erreur("Incohérence interne (corr_varprim)","type de solveur inconnu")
endselect 

endsubroutine corr_varprim

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

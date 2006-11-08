!------------------------------------------------------------------------------!
! Procedure : corr_varprim                Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  :
!   Calcul des variables primitives aux frontieres de couplage,
!   tenant compte des corrections de flux. Orientation selon le solver.
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine corr_varprim(field, domaine, def_solver, dif_enflux, nb, part_cor, &
                        typ_cor, fincycle)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use MENU_SOLVER
use DEFFIELD
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrees --
type(st_ustmesh)      :: domaine          ! domaine non structure a integrer
type(mnu_solver)      :: def_solver     ! proprietes du solver
type(st_genericfield) :: dif_enflux       ! energie a ajouter, pour correction de flux
integer               :: nb               ! index de la condition aux limites
real(krp)             :: part_cor         ! coefficient donnant la part de la 
                                          ! correction a apporter
integer               :: typ_cor          ! type de correction
logical               :: fincycle

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ des valeurs et residus

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(def_solver%typ_solver)
case(solKDIF)
  call corr_varprim_kdif(field, domaine, def_solver, dif_enflux, nb, &
                         part_cor, typ_cor, fincycle)
case(solNS)

case default
  call erreur("Incoherence interne (corr_varprim)","type de solveur inconnu")
endselect 

endsubroutine corr_varprim

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

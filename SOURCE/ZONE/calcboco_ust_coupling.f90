!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_coupling       Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : 
!   Conditions aux limites de couplage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_coupling(defboco, ustboco, ustdom, champ, condrac, solvercoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrées --
type(mnu_boco)   :: defboco           ! paramètres de conditions aux limites
type(st_ustboco) :: ustboco           ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom            ! maillage non structuré
type(st_genericfield) :: condrac      ! stockage des conditions limites de couplage
integer          :: solvercoupling

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états

! -- Declaration des variables internes --

! -- Debut de la procedure --
select case(defboco%typ_calc)

case(bc_calc_flux)
  !call calcboco_ust_coupling_flux(ustboco, ustdom, champ, condrac, &
  !                                solvercoupling)
  call calcboco_ust_coupling_face(ustboco, ustdom, champ, condrac, &
                                  solvercoupling)
case(bc_calc_ghostface)
  call calcboco_ust_coupling_face(ustboco, ustdom, champ, condrac, &
                                  solvercoupling)
case(bc_calc_ghostcell)
  !call calcboco_ust_coupling_cell(ustboco, ustdom, champ, condrac, &
  !                                solvercoupling)
case default
  call erreur("Lecture de menu raccord","méthode de calcul de raccord non reconnue")  

endselect

endsubroutine calcboco_ust_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

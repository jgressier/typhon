!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_coupling       Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : 
!   Conditions aux limites de couplage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_coupling(defboco, ustboco, umesh, champ, condrac, solvercoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
type(mnu_boco)   :: defboco           ! parametres de conditions aux limites
type(st_ustboco) :: ustboco           ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh             ! unstructured mesh
type(st_genericfield) :: condrac      ! stockage des conditions limites de couplage
integer          :: solvercoupling

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des etats

! -- Declaration des variables internes --

! -- Debut de la procedure --
select case(defboco%typ_calc)

case(bc_calc_flux)
  !call calcboco_ust_coupling_flux(ustboco, umesh, champ, condrac, &
  !                                solvercoupling)
  call calcboco_ust_coupling_face(ustboco, umesh, champ, condrac, &
                                  solvercoupling)
case(bc_calc_ghostface)
  call calcboco_ust_coupling_face(ustboco, umesh, champ, condrac, &
                                  solvercoupling)
case(bc_calc_ghostcell)
  !call calcboco_ust_coupling_cell(ustboco, umesh, champ, condrac, &
  !                                solvercoupling)
case default
  call erreur("Lecture de menu raccord","methode de calcul de raccord non reconnue")  

endselect

endsubroutine calcboco_ust_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : calcboco_ust_coupling_face  Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : 
!   Conditions aux limites de couplage, methode du flux de face
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust_coupling_face(ustboco, ustdom, champ, condrac, solvercoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD
use VARCOM

implicit none

! -- Declaration des entrees --
type(st_ustboco) :: ustboco           ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom            ! maillage non structure
type(st_genericfield) :: condrac ! stockage des conditions limites de couplage
integer          :: solvercoupling   

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des etats

! -- Declaration des variables internes --

! -- Debut de la procedure --
select case(solvercoupling)
case(kdif_kdif)
  call calcboco_kdif_coupling_face(ustboco, ustdom, champ, condrac)

case(kdif_ns)
 call erreur("incoherence interne (calcboco_ust_coupling_face)", &
             "non implemente")

case(ns_ns)
 call erreur("incoherence interne (calcboco_ust_coupling_face)", &
             "non implemente")

case default
 call erreur("incoherence interne (calcboco_ust_coupling_face)", &
             "couplage de solvers inconnu")
endselect

endsubroutine calcboco_ust_coupling_face

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : init_coupling               Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Initialisation des structures de données d'échange et de résultats de
!   couplage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_coupling(zone1, zone2, nbc1, nbc2, ncoupl1, ncoupl2, raccord)

use TYPHMAKE
use DEFZONE
use OUTPUT
use VARCOM

implicit none

! -- Declaration des entrées --
integer :: nbc1, nbc2  ! indices de conditions limites pour les zones 1 et 2
integer :: ncoupl1, ncoupl2 ! numéro de raccord pour les zones 1 et 2
integer :: raccord     ! type de conditions limites au raccord

! -- Declaration des sorties --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone1, zone2

! -- Declaration des variables internes --
integer       :: ic, ib
integer       :: typsolver1, typsolver2

! -- Debut de la procedure --

! Détermination des solveurs des deux zones couplées

typsolver1 = zone1%defsolver%typ_solver
typsolver2 = zone2%defsolver%typ_solver

select case(typsolver1)

 case(solKDIF)
  select case(typsolver2)
   case(solKDIF)
    zone1%coupling(ncoupl1)%zcoupling%solvercoupling = kdif_kdif
    zone2%coupling(ncoupl2)%zcoupling%solvercoupling = kdif_kdif
   case(solNS)
    zone1%coupling(ncoupl1)%zcoupling%solvercoupling = kdif_ns
    zone2%coupling(ncoupl2)%zcoupling%solvercoupling = kdif_ns
   case default
    call erreur("incohérence interne (init_coupling)", "solveur inconnu")
  endselect
 
 case(solNS)
  select case(typsolver2)
   case(solKDIF)
    zone1%coupling(ncoupl1)%zcoupling%solvercoupling = kdif_ns
    zone2%coupling(ncoupl2)%zcoupling%solvercoupling = kdif_ns
   case(solNS)
    zone1%coupling(ncoupl1)%zcoupling%solvercoupling = ns_ns
    zone2%coupling(ncoupl2)%zcoupling%solvercoupling = ns_ns
   case default
    call erreur("incohérence interne (init_coupling)", "solveur inconnu")
  endselect

endselect

! Initialisation des structures du couplage

call new(zone1%coupling(ncoupl1)%zcoupling, zone1%grid%umesh%boco(nbc1)%nface)
call new(zone2%coupling(ncoupl2)%zcoupling, zone2%grid%umesh%boco(nbc2)%nface)

! calcul des connections et connectivités entre zones
! maillages coincidants
call calc_connface(zone1%grid%umesh, zone1%grid%umesh%boco(nbc1), &
                   zone1%coupling(ncoupl1)%zcoupling%connface, &
                   zone2%grid%umesh, zone2%grid%umesh%boco(nbc2), &
                   zone2%coupling(ncoupl2)%zcoupling%connface)

  write(uf_log,"(a,5i)")"conn de zones : ", &
                     zone1%coupling(ncoupl1)%zcoupling%connface(1:5:1)!! DEBUG

! Initialisation des conditions aux limites au raccord
call update_couplingboco(zone1, zone2, nbc1, nbc2, ncoupl1, raccord)

endsubroutine init_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 : création de la procédure
! avr  2004 : modification par intégration MGRID (grille unique)
!------------------------------------------------------------------------------!

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

subroutine init_coupling(zone1, zone2)

use TYPHMAKE
use DEFZONE
use OUTPUT
use VARCOM

implicit none

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone1, zone2

! -- Declaration des variables internes --
integer       :: ic, ib
integer       :: ncoupl1, ncoupl2
integer       :: nbc1, nbc2
integer       :: typsolver1, typsolver2

! -- Debut de la procedure --

! Détermination des numéros du raccord pour les zones 1 et 2

do ic = 1, zone1%ncoupling
  if (samestring(zone1%coupling(ic)%connzone, zone2%nom)) then
    ncoupl1 = ic
  endif
enddo

do ic = 1, zone2%ncoupling
  if (samestring(zone2%coupling(ic)%connzone, zone1%nom)) then
    ncoupl2 = ic
  endif
enddo
  
! Détermination des indices de condition aux limites pour les zones 1 et 2

do ib = 1, zone1%grid%umesh%nboco
  if (samestring(zone1%coupling(ncoupl1)%family, zone1%grid%umesh%boco(ib)%family)) then
    nbc1 = ib
  endif
enddo

do ib = 1, zone2%grid%umesh%nboco
  if (samestring(zone2%coupling(ncoupl2)%family, zone2%grid%umesh%boco(ib)%family)) then
    nbc2 = ib
  endif
enddo

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
endsubroutine init_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 : création de la procédure
! avr  2004 : modification par intégration MGRID (grille unique)
!------------------------------------------------------------------------------!

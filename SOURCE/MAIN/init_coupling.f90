!------------------------------------------------------------------------------!
! Procedure : init_coupling               Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Initialisation des structures de donn�es d'�change et de r�sultats de
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

! -- Declaration des entr�es --

! -- Declaration des sorties --

! -- Declaration des entr�es/sorties --
type(st_zone) :: zone1, zone2

! -- Declaration des variables internes --
integer       :: ic, ib
integer       :: ncoupl1, ncoupl2
integer       :: nbc1, nbc2
integer       :: typsolver1, typsolver2

! -- Debut de la procedure --

! D�termination des num�ros du raccord pour les zones 1 et 2

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
  
! D�termination des indices de condition aux limites pour les zones 1 et 2

do ib = 1, zone1%ust_mesh%nboco
  if (samestring(zone1%coupling(ncoupl1)%family, zone1%ust_mesh%boco(ib)%family)) then
    nbc1 = ib
  endif
enddo

do ib = 1, zone2%ust_mesh%nboco
  if (samestring(zone2%coupling(ncoupl2)%family, zone2%ust_mesh%boco(ib)%family)) then
    nbc2 = ib
  endif
enddo

! D�termination des solveurs des deux zones coupl�es

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
    call erreur("incoh�rence interne (init_coupling)", "solveur inconnu")
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
    call erreur("incoh�rence interne (init_coupling)", "solveur inconnu")
  endselect

endselect

! Initialisation des structures du couplage

call new(zone1%coupling(ncoupl1)%zcoupling, zone1%ust_mesh%boco(nbc1)%nface)
call new(zone2%coupling(ncoupl2)%zcoupling, zone2%ust_mesh%boco(nbc2)%nface)

! calcul des connections et connectivit�s entre zones
! maillages coincidants
call calc_connface(zone1%ust_mesh, zone1%ust_mesh%boco(nbc1), &
                   zone1%coupling(ncoupl1)%zcoupling%connface, &
                   zone2%ust_mesh, zone2%ust_mesh%boco(nbc2), &
                   zone2%coupling(ncoupl2)%zcoupling%connface)

endsubroutine init_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b) : cr�ation de la proc�dure
!------------------------------------------------------------------------------!

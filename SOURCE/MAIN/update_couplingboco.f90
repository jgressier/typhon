!------------------------------------------------------------------------------!
! Procedure : update_couplingboco         Auteur : J. Gressier/ E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Mise jour des conditions limites de raccord
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine update_couplingboco(zone1, zone2, nbc1, nbc2, ncoupl1, raccord)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées/sorties --
type(st_zone) :: zone1, zone2

! -- Declaration des entrées --
integer       :: nbc1, nbc2, ncoupl1, raccord

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(zone1%coupling(ncoupl1)%zcoupling%solvercoupling)

case(kdif_kdif)
  call update_couplingboco_kdif(raccord, zone1%defsolver%boco(zone1%grid%umesh%boco(nbc1)%idefboco)%typ_boco, &
                  zone2%defsolver%boco(zone2%grid%umesh%boco(nbc2)%idefboco)%typ_boco)

  case default
  call erreur("update_couplingboco","cas non implémenté")

endselect


endsubroutine update_couplingboco

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin  2004 : création de la procédure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : update_couplingboco         Auteur : J. Gressier/ E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Update of coupling boundary conditions
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

! -- Declaration des entrees/sorties --
type(st_zone) :: zone1, zone2

! -- Declaration des entrees --
integer       :: nbc1, nbc2, ncoupl1, raccord

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --

!select case(zone1%coupling(ncoupl1)%zcoupling%solvercoupling)

!case(kdif_kdif)

select case(raccord)
case(couplingboco_TT)
  zone1%defsolver%boco(zone1%grid%umesh%boco(nbc1)%idefboco)%typ_boco = &
                                                                  bc_wall_isoth
  zone2%defsolver%boco(zone2%grid%umesh%boco(nbc2)%idefboco)%typ_boco = &
                                                                  bc_wall_isoth

case(couplingboco_CC)
  zone1%defsolver%boco(zone1%grid%umesh%boco(nbc1)%idefboco)%typ_boco = &
                                                                  bc_wall_hconv
  zone2%defsolver%boco(zone2%grid%umesh%boco(nbc2)%idefboco)%typ_boco = &
                                                                  bc_wall_hconv

case(couplingboco_CT)
  zone1%defsolver%boco(zone1%grid%umesh%boco(nbc1)%idefboco)%typ_boco = &
                                                                  bc_wall_hconv
  zone2%defsolver%boco(zone2%grid%umesh%boco(nbc2)%idefboco)%typ_boco = &
                                                                  bc_wall_isoth

case(couplingboco_TC)
  zone1%defsolver%boco(zone1%grid%umesh%boco(nbc1)%idefboco)%typ_boco = &
                                                                  bc_wall_isoth
  zone2%defsolver%boco(zone2%grid%umesh%boco(nbc2)%idefboco)%typ_boco = &
                                                                  bc_wall_hconv

case default
  call erreur("update_couplingboco_kdif","Conditions limites de raccord inconnues")
endselect

!  case default
!  call erreur("update_couplingboco","cas non implemente")

!endselect


endsubroutine update_couplingboco

!------------------------------------------------------------------------------!
! Historique des modifications
!
! june 2004 : creation of procedure 
! june 2005 : generalization to KDIF - NS couplings
!------------------------------------------------------------------------------!

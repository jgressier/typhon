!------------------------------------------------------------------------------!
! Procedure : update_couplingboco_kdif    Auteur : J. Gressier/ E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Mise jour des conditions limites de raccord
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine update_couplingboco_kdif(raccord, typ_boco1, typ_boco2)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees/sorties --
integer :: raccord
integer :: typ_boco1, typ_boco2

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(raccord)
case(couplingboco_TT)
  typ_boco1 = bc_wall_isoth
  typ_boco2 = bc_wall_isoth

case(couplingboco_CC)
  typ_boco1 = bc_wall_hconv
  typ_boco2 = bc_wall_hconv

case(couplingboco_CT)
  typ_boco1 = bc_wall_hconv
  typ_boco2 = bc_wall_isoth

case(couplingboco_TC)
  typ_boco1 = bc_wall_isoth
  typ_boco2 = bc_wall_hconv

case default
  call erreur("update_couplingboco_kdif","Conditions limites de raccord inconnues")
endselect


endsubroutine update_couplingboco_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin  2004 : creation de la procedure
!------------------------------------------------------------------------------!

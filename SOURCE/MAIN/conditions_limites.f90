!------------------------------------------------------------------------------!
! Procedure : conditions_limites          Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions limites pour une zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine conditions_limites(lzone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MGRID

implicit none

! -- Declaration des entrees --
type(st_zone) :: lzone            ! zone a integrer

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: ifield            ! indice de champ

! -- Debut de la procedure --

pgrid => lzone%grid

do while(associated(pgrid))

  call calcboco_ust(lzone%defsolver, pgrid, lzone%defspat)
  pgrid => pgrid%next

enddo

endsubroutine conditions_limites

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003 : creation de la procedure
! avr  2004 : suppression de STRMESH / boucle sur MGRID
!------------------------------------------------------------------------------!

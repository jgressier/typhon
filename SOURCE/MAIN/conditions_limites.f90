!------------------------------------------------------------------------------!
! Procedure : conditions_limites                  Authors : J. Gressier
!                                                 Date    : March 2003
! Fonction
!   Calcul des conditions limites pour une zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine conditions_limites(defsolver, gridlist)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MGRID

implicit none

! -- INPUTS --    
type(mnu_solver)  :: defsolver           ! solver parameters
type(st_gridlist) :: gridlist            ! zone a integrer

! -- OUTPUTS --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: ifield            ! indice de champ

! -- Debut de la procedure --

pgrid => gridlist%first

do while(associated(pgrid))

  call calcboco_ust(defsolver, pgrid, defsolver%defspat)
  pgrid => pgrid%next

enddo

endsubroutine conditions_limites

!------------------------------------------------------------------------------!
! Changes History
!
! avr  2003 : creation de la procedure
! avr  2004 : suppression de STRMESH / boucle sur MGRID
! mar  2006 : loop on gridlist
!------------------------------------------------------------------------------!

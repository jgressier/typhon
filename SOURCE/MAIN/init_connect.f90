!------------------------------------------------------------------------------!
! Procedure : init_connect                Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction 
!   Initialisation des connectivites des conditions limites
!
!------------------------------------------------------------------------------!
subroutine init_connect(zone)

use TYPHMAKE
use VARCOM     ! Definition des constantes
use OUTPUT
use DEFZONE

implicit none

! -- INPUTS --

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- OUTPUTS --

! -- Internal variables --
type(st_grid), pointer :: pgrid

! -- BODY --

pgrid => zone%gridlist%first

do while (associated(pgrid))

  select case(zone%defsolver%typ_solver)

  case(solVORTEX, solNS, solKDIF)
    call init_connect_grid(zone%defsolver, pgrid)
    
  case default
  endselect

   pgrid => pgrid%next

enddo

endsubroutine init_connect

!------------------------------------------------------------------------------!
! Changes history
!
! mar  2003 : creation de la procedure
! mar  2004 : ajout du traitement GRID (solveur VORTEX)
! Aug  2005 : remove call to init_connect_ust (KDIF), changed to init_connect_grid
!------------------------------------------------------------------------------!

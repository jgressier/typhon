!------------------------------------------------------------------------------!
! Procedure : init_connect                Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des connectivites des conditions limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_connect(zone)

use TYPHMAKE
use VARCOM     ! Definition des constantes
use OUTPUT
use DEFZONE

implicit none

! -- Declaration des entrees --

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid

! -- Debut de la procedure --

pgrid => zone%grid

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

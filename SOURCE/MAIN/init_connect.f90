!------------------------------------------------------------------------------!
! Procedure : init_connect                Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des connectivités des conditions limites
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

! -- Declaration des entrées --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid

! -- Debut de la procedure --

pgrid => zone%grid

do while (associated(pgrid))

  select case(zone%defsolver%typ_solver)
  case(solKDIF)
    call  init_connect_ust(zone%defsolver, pgrid%umesh)

  case(solVORTEX, solNS)
    call init_connect_grid(zone%defsolver, pgrid)
    
  case default
  endselect

   pgrid => pgrid%next

enddo

endsubroutine init_connect

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : création de la procédure
! mars 2004 : ajout du traitement GRID (solveur VORTEX)
!------------------------------------------------------------------------------!

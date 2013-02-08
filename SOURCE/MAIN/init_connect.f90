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

  solver: select case(zone%defsolver%typ_solver)

  case(solVORTEX, solNS, solKDIF)
  
    gridtype: select case(pgrid%info%gridtype)
    case(grid_ust)
      call init_connect_grid(zone%defsolver, pgrid) ! must be renamed to UST treatment

    case(grid_str)     
      ! DEV : nothing to fdo (for now)
      
    case default
      call error_stop("unknown type of grid (init_connect)")
    endselect gridtype
      
  case default
  endselect solver

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

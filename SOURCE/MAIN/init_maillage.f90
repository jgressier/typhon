!------------------------------------------------------------------------------!
! Procedure : init_maillage               Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Calcul et initialisation du maillage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_maillage(zone)

use TYPHMAKE
use VARCOM
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

select case(zone%defsolver%typ_solver)

case(solKDIF)
  call calc_ustmesh(zone%grid%umesh)

case(solVORTEX)
  pgrid => zone%grid
  do while (associated(pgrid))
    call calc_ustmesh(pgrid%umesh)
    pgrid => pgrid%next
  enddo
    
case default
endselect

endsubroutine init_maillage

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : création de la procédure
! mars 2004 : traitement des grilles (VORTEX)
!------------------------------------------------------------------------------!


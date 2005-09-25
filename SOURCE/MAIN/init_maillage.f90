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

! -- Declaration des entrees --

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid

! -- Debut de la procedure --

select case(zone%defsolver%typ_solver)

case(solKDIF)
  call calc_ustmesh(zone%grid%umesh, zone%defmesh)

case(solVORTEX, solNS)
  pgrid => zone%grid
  do while (associated(pgrid))
    call calc_ustmesh(pgrid%umesh, zone%defmesh)
    pgrid => pgrid%next
  enddo
    
case default

  call erreur('Developpement','cas de solveur non prevu (init_maillage)')

endselect

endsubroutine init_maillage

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation de la procedure
! mars 2004 : traitement des grilles (VORTEX)
!------------------------------------------------------------------------------!


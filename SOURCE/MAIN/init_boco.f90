!------------------------------------------------------------------------------!
! Procedure : init_boco                   Auteur : J. Gressier/ E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des conditions limites
!
!------------------------------------------------------------------------------!

subroutine init_boco(zone)

use TYPHMAKE
use VARCOM
use OUTPUT
use DEFZONE
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
type(st_zone)    :: zone               ! maillage et connectivites

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: ig

! -- Debut de la procedure --

! initialisation selon solveur

select case(zone%defsolver%typ_solver)

case(solNS)
  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    call init_boco_ns(zone%defsolver, pgrid%umesh)
    pgrid => pgrid%next
  enddo

case(solKDIF)
  if (zone%gridlist%nbgrid /= 1) call erreur("Init BOCO","une seule grille acceptee")
  call init_boco_kdif(zone%defsolver, zone%gridlist%first%umesh)

case(solVORTEX)
  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    call init_boco_vort(zone%defsolver, pgrid)
    pgrid => pgrid%next
  enddo

case default
  call erreur("Incoherence interne (init_boco_ust)","type de solveur inconnu")
endselect 

endsubroutine init_boco

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2003 : creation de la procedure
! mars 2004 : fusion "init_boco_ust" dans "init_boco"
!             ajout du solveur VORTEX
! july 2004 : NS solver (call init_boco_ns)
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Procedure : init_boco                   Auteur : J. Gressier/ E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des conditions limites
!
! Defauts/Limitations/Divers :
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

! -- Declaration des entrées --
type(st_zone)    :: zone               ! maillage et connectivités

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: ig

! -- Debut de la procedure --

! initialisation selon solveur

select case(zone%defsolver%typ_solver)

case(solNS)
  pgrid => zone%grid
  do ig = 1, zone%ngrid
    call init_boco_ns(zone%defsolver, pgrid)
    pgrid => pgrid%next
  enddo

case(solKDIF)
  if (zone%ngrid /= 1) call erreur("Init BOCO","une seule grille acceptée")
  call init_boco_kdif(zone%defsolver, zone%grid%umesh)

case(solVORTEX)
  pgrid => zone%grid
  do ig = 1, zone%ngrid
    call init_boco_vort(zone%defsolver, pgrid)
    pgrid => pgrid%next
  enddo

case default
  call erreur("Incohérence interne (init_boco_ust)","type de solveur inconnu")
endselect 

endsubroutine init_boco

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003 : création de la procédure
! mars 2004 : fusion "init_boco_ust" dans "init_boco"
!             ajout du solveur VORTEX
! july 2004 : NS solver (call init_boco_ns)
!------------------------------------------------------------------------------!


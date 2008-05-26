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

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
type(st_grid), pointer :: pgrid
type(st_ustmesh)       :: newmesh
real(krp)              :: alpha, beta      ! geometric coefficient for the split
! -- BODY --

select case(zone%defsolver%typ_solver)

case(solKDIF)
  call calc_ustmesh(zone%gridlist%first%umesh, zone%defmesh)

case(solVORTEX, solNS)
  pgrid => zone%gridlist%first

  do while (associated(pgrid))

    ! -- if needed, split mesh into spectral volume subcells --

    select case(zone%defmesh%splitmesh)
    case(split_none)
      ! nothing to do
    case(split_svm2quad)
      call convert_to_svm(zone%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh)
      call delete(pgrid%umesh)
      pgrid%umesh = newmesh
    case(split_svm3wang)
     alpha= 1._krp /  4._krp
     beta= 1._krp /  3._krp
      call convert_to_svm_cub(zone%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh,alpha,beta)
      call delete(pgrid%umesh)
      pgrid%umesh = newmesh
    case(split_svm3kris)
     alpha= 91._krp /  1000._krp
     beta= 18._krp /  100._krp
      call convert_to_svm_cub(zone%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh,alpha,beta)
      call delete(pgrid%umesh)
      pgrid%umesh = newmesh
    case(split_svm3kris2)
     alpha= 0.1093621117
     beta= 0.1730022492
      call convert_to_svm_cub(zone%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh,alpha,beta)
      call delete(pgrid%umesh)
      pgrid%umesh = newmesh
    case default
      call erreur('Development','unknown splitting method (init_maillage)')
    endselect

    ! -- compute geometrical properties of CELLS / FACES of the mesh --

    call calc_ustmesh(pgrid%umesh, zone%defmesh)

    pgrid => pgrid%next

  enddo
    
case default

  call erreur('Development','unknown solver (init_maillage)')

endselect

endsubroutine init_maillage

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002: creation de la procedure
! mars 2004: traitement des grilles (VORTEX)
! oct  2007: add conversion to "Spectral Volume" mesh
!------------------------------------------------------------------------------!


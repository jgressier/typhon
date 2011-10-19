!------------------------------------------------------------------------------!
! Procedure : init_maillage               Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Calcul et initialisation du maillage
!
!------------------------------------------------------------------------------!
subroutine initzone_mesh(zone)

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
real(krp)              :: alpha, beta,gamma,delta      ! geometric coefficient for the split
integer(kip)           :: isplit

! -- BODY --

  pgrid => zone%gridlist%first

  do while (associated(pgrid))

    ! -- if needed, split mesh into spectral volume subcells --

    pgrid%umesh_legacy = pgrid%umesh

    select case(zone%defsolver%defmesh%splitmesh)
    case(split_none)
      ! nothing to do
    case(split_svm2quad)
      call convert_to_svm(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh)
    case(split_svm3wang)
      alpha = 1._krp /  4._krp
      beta  = 1._krp /  3._krp
      call convert_to_svm_cub(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh, alpha,beta)
    case(split_svm3kris)
      alpha = 91._krp /  1000._krp
      beta  = 18._krp /  100._krp
      call convert_to_svm_cub(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh, alpha, beta)
    case(split_svm3kris2)
      alpha = 0.1093621117_krp
      beta  = 0.1730022492_krp
      call convert_to_svm_cub(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh, alpha, beta)
    case(split_svm4wang)
      call convert_to_svm_4wang(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh)
    case(split_svm4kris)
      alpha = 78._krp / 1000._krp
      beta  = 104._krp / 1000._krp
      gamma = 52._krp / 1000._krp
      delta = 351._krp / 1000._krp
      call convert_to_svm_4kris(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh, alpha,beta,gamma,delta)
    case(split_svm4kris2)
      alpha = 0.0326228301_krp
      beta  = 0.042508082_krp
      gamma = 0.0504398911_krp
      delta = 0.1562524902_krp
      call convert_to_svm_4kris(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh_legacy, pgrid%umesh, alpha,beta,gamma,delta)
    case(split_iso_tri)
      do isplit=1, zone%defsolver%defmesh%nsplit
        call raffin_iso_tri(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh)
        call delete(pgrid%umesh)
        pgrid%umesh = newmesh
      enddo
    case(split_iso_quad)
      do isplit=1, zone%defsolver%defmesh%nsplit
        call raffin_iso_quad(zone%defsolver%defmesh, zone%defsolver%defspat, pgrid%umesh, newmesh)
        call delete(pgrid%umesh)
        pgrid%umesh = newmesh
      enddo
    case default
      call error_stop('Internal error: unknown splitting method (initzone_mesh)')
    endselect

    ! -- compute geometrical properties of CELLS / FACES of the mesh --

    call calc_ustmesh(pgrid%umesh, zone%defsolver%defmesh)

    pgrid => pgrid%next

  enddo

  !--------------------------------------------------------
  ! compute zone geometrical properties

  zone%info%totvolume = 0._krp

  ! -- loop on grids of this thread --

  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    zone%info%totvolume = zone%info%totvolume + sum(pgrid%umesh%mesh%volume(1:pgrid%umesh%ncell_int, 1, 1))
    pgrid => pgrid%next
  enddo

  ! -- reduce (sum) on all threads
  call allreduce_sum(zone%info%totvolume)
    
endsubroutine initzone_mesh
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002: creation de la procedure
! mars 2004: traitement des grilles (VORTEX)
! oct  2007: add conversion to "Spectral Volume" mesh
! June 2009: change name to initzone_mesh (from init_maillage), compute volume
!------------------------------------------------------------------------------!


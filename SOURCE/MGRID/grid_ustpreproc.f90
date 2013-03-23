!------------------------------------------------------------------------------!
! Procedure : grid_ustpreproc              Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Calcul et initialisation du maillage
!
!------------------------------------------------------------------------------!
subroutine grid_ustpreproc(defsolver, grid)

use TYPHMAKE
use MENU_SOLVER
use MESHPARAMS
use OUTPUT

implicit none

! -- INPUTS/OUTPUTS --
type(mnu_solver) :: defsolver

! -- INPUTS/OUTPUTS --
type(st_grid) :: grid

! -- Internal variables --
type(st_ustmesh)       :: newmesh
real(krp)              :: alpha, beta, gamma, delta      ! geometric coefficient for the split
integer(kip)           :: isplit

! -- BODY --

grid%umesh_legacy = grid%umesh

! -- if needed, split mesh into spectral volume subcells --

    select case(defsolver%defmesh%defsplit%splitmesh)
    case(split_none)
      ! nothing to do
    case(split_svm2quad)
      call convert_to_svm(defsolver%defmesh, grid%umesh_legacy, grid%umesh)
    case(split_svm3wang)
      alpha = 1._krp /  4._krp
      beta  = 1._krp /  3._krp
      call convert_to_svm_cub(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh, alpha,beta)
    case(split_svm3kris)
      alpha = 91._krp /  1000._krp
      beta  = 18._krp /  100._krp
      call convert_to_svm_cub(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh, alpha, beta)
    case(split_svm3kris2)
      alpha = 0.1093621117_krp
      beta  = 0.1730022492_krp
      call convert_to_svm_cub(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh, alpha, beta)
    case(split_svm4wang)
      call convert_to_svm_4wang(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh)
    case(split_svm4kris)
      alpha = 78._krp / 1000._krp
      beta  = 104._krp / 1000._krp
      gamma = 52._krp / 1000._krp
      delta = 351._krp / 1000._krp
      call convert_to_svm_4kris(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh, alpha,beta,gamma,delta)
    case(split_svm4kris2)
      alpha = 0.0326228301_krp
      beta  = 0.042508082_krp
      gamma = 0.0504398911_krp
      delta = 0.1562524902_krp
      call convert_to_svm_4kris(defsolver%defmesh, defsolver%defspat, grid%umesh_legacy, grid%umesh, alpha,beta,gamma,delta)
    case(split_iso_tri)
      do isplit = 1, defsolver%defmesh%defsplit%nsplit
        call raffin_iso_tri(defsolver%defmesh, defsolver%defspat, grid%umesh, newmesh)
        if (isplit > 1) call delete(grid%umesh)
        grid%umesh = newmesh
      enddo
    case(split_quad2x2)
      do isplit = 1, defsolver%defmesh%defsplit%nsplit
        call splitquadto2x2(defsolver%defmesh, grid%umesh, newmesh)
        if (isplit > 1) call delete(grid%umesh)
        grid%umesh = newmesh
      enddo
    case(split_quad3x3)
      do isplit = 1, defsolver%defmesh%defsplit%nsplit
        call splitquadto3x3(defsolver%defmesh, grid%umesh, newmesh)
        if (isplit > 1) call delete(grid%umesh)
        grid%umesh = newmesh
      enddo
    case(split_quad3x3lg)
      call splitquadto3x3(defsolver%defmesh, grid%umesh_legacy, grid%umesh)
    case default
      call error_stop('Internal error: unknown splitting method (grid_ustpreproc)')
    endselect

  ! -- compute geometrical properties of CELLS / FACES of the mesh --

  call calc_ustmesh(defsolver%defmesh, defsolver%fctenv, grid%umesh)
    
endsubroutine grid_ustpreproc
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002: creation de la procedure
! mars 2004: traitement des grilles (VORTEX)
! oct  2007: add conversion to "Spectral Volume" mesh
! June 2009: change name to initzone_mesh   (from init_maillage), compute volume
! Feb  2013: change name to grid_ustpreproc (from initzone_mesh)
!------------------------------------------------------------------------------!


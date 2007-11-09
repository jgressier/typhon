!------------------------------------------------------------------------------!
! Procedure : tstep_implicit                            Auteur : J. Gressier
!                                                      Date   : Avril 2004
! Fonction
!   Implicit Integration of the domain
!
!------------------------------------------------------------------------------!
subroutine tstep_implicit(dtloc, typtemps, defsolver, &
                         umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MATRIX_ARRAY
use SPARSE_MAT
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(st_ustmesh) :: umesh      ! domaine non structure a integrer
real(krp)        :: dtloc(1:umesh%ncell)         ! pas de temps CFL
integer          :: ncp        ! nombre de couplages de la zone

! -- Input/output --
type(st_field)   :: field            ! champ des valeurs et residus
type(mnu_zonecoupling), dimension(1:ncp) &
                 :: coupling ! donnees de couplage

! -- Internal variables --
type(st_spmat)         :: mat
type(st_genericfield)  :: flux             ! tableaux des flux
type(st_mattab)        :: jacL, jacR       ! tableaux de jacobiennes des flux
real(krp), allocatable :: tabres(:)        ! collect residuals
integer(kip)           :: if, ic1, ic2, ic, info, dim

! -- BODY --

call new(jacL, umesh%nface, defsolver%nequat)
call new(jacR, umesh%nface, defsolver%nequat)

!--------------------------------------------------
! phase explicite : right hand side computation
!--------------------------------------------------

! -- allocation des flux et termes sources --

call new(flux, umesh%nface, field%nscal, field%nvect, 0)

select case(defsolver%typ_solver)
case(solKDIF)
  call integration_kdif_ust(defsolver, defsolver%defspat, umesh, field, flux, .true., jacL, jacR)
case(solNS)
  call integration_ns_ust(defsolver, defsolver%defspat, umesh, field, flux, .true., jacL, jacR)
case default
  call erreur("internal error (tstep_implicit)", "unknown or unexpected solver")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dtloc, umesh, flux, field%residu, .true., jacL, jacR)

call delete(flux)

!--------------------------------------------------
! build implicit system
!--------------------------------------------------

call build_implicit(dtloc, defsolver%deftime, umesh, jacL, jacR, mat, field%residu)

call delete(jacL)
call delete(jacR)

!--------------------------------------------------
! solve implicit system
!--------------------------------------------------

call implicit_solve(defsolver%deftime, mat, field%residu)

call delete(mat)

!--------------------------------------------------


endsubroutine tstep_implicit
!------------------------------------------------------------------------------!
! Change History
!
! Apr  2004 : creation
! Aug  2005 : split / call build_implicit to handle different structures
!           : split / call implicit_solve 
! sept 2005 : local time stepping
! Nov  2007 : new name "tstep_implicit"
!------------------------------------------------------------------------------!

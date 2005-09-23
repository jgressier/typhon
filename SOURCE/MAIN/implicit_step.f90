!------------------------------------------------------------------------------!
! Procedure : implicit_step                            Auteur : J. Gressier
!                                                      Date   : Avril 2004
! Fonction
!   Implicit Integration of the domain
!
!------------------------------------------------------------------------------!
subroutine implicit_step(dtloc, typtemps, defsolver, defspat, deftime, &
                         umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use MATRIX_ARRAY
use SPARSE_MAT
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(mnu_spat)   :: defspat    ! parametres d'integration spatiale
type(mnu_time)   :: deftime    ! parametres d'integration spatiale
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
  call integration_kdif_ust(defsolver, defspat, umesh, field, flux, .true., jacL, jacR)
case(solNS)
  call integration_ns_ust(defsolver, defspat, umesh, field, flux, .true., jacL, jacR)
case default
  call erreur("internal error (implicit_step)", "unknown or unexpected solver")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dtloc, umesh, flux, field%residu, .true., jacL, jacR)

call delete(flux)

!--------------------------------------------------
! build implicit system
!--------------------------------------------------

call build_implicit(dtloc, deftime, umesh, jacL, jacR, mat, field%residu)

call delete(jacL)
call delete(jacR)

!--------------------------------------------------
! solve implicit system
!--------------------------------------------------

call implicit_solve(deftime, mat, field%residu)

call delete(mat)

!--------------------------------------------------

!select case(typtemps)
! case(instationnaire) ! corrections de flux seulement en instationnaire

! ! Calcul de l'"energie" a l'interface, en vue de la correction de flux, pour 
! ! le couplage avec echanges espaces
! !DVT : flux%tabscal(1) !
! if (ncp>0) then
!   call accumulfluxcorr(dt, defsolver, umesh%nboco, umesh%boco, &
!                        umesh%nface, flux%tabscal(1)%scal, ncp, &
!                        coupling)
! endif

!endselect

!if (ncp > 0) call erreur("Developpement","couplage interdit en implicite")


endsubroutine implicit_step
!------------------------------------------------------------------------------!
! Change History
!
! Apr  2004 : creation
! Aug  2005 : split / call build_implicit to handle different structures
!           : split / call implicit_solve 
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!

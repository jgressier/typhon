!------------------------------------------------------------------------------!
! Procedure : tstep_explicit 
!
! Fonction
!   Integration explicit de domaine
!
!------------------------------------------------------------------------------!
subroutine tstep_explicit(dtloc, typtemps, defsolver, umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MENU_ZONECOUPLING
use MATRIX_ARRAY
use MENU_GEN

implicit none

! -- INPUTS --
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(st_ustmesh) :: umesh      ! domaine non structure a integrer
real(krp)        :: dtloc(1:umesh%ncell)         ! pas de temps CFL
integer          :: ncp        ! nombre de couplages de la zone

! -- INPUTS/OUTPUTS --
type(st_field)   :: field            ! champ des valeurs et residus
type(mnu_zonecoupling), dimension(1:ncp) &
                 :: coupling ! donnees de couplage

! -- Internal variables --
type(st_genericfield) :: flux             ! tableaux des flux
type(st_mattab)       :: jacL, jacR       ! tableaux FICTIFS de jacobiennes des flux

! -- BODY --


! -- allocation des flux et termes sources (structure equivalente a field%etatcons) --

call new(flux, umesh%nface, field%nscal, field%nvect, 0)

! On peut ici decouper le maillage complet en blocs de taille fixe pour optimiser
! l'encombrement memoire et la vectorisation

select case(defsolver%typ_solver)
case(solNS)
  call integration_ns_ust(defsolver, defsolver%defspat, umesh, field, flux, .false., jacL, jacR)
case(solKDIF)
  call integration_kdif_ust(defsolver, defsolver%defspat, umesh, field, flux, .false., jacL, jacR)
case default
  call erreur("incoherence interne (tstep_explicit)", "solveur inconnu")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dtloc, umesh, flux, field%residu, .false., jacL, jacR)

!-----------------------------------------------------------------------
! BOCO HISTORY
!-----------------------------------------------------------------------

call integ_ustboco(umesh, field, flux) 

!-----------------------------------------------------------------------
! COUPLING SPECIFIC actions
!-----------------------------------------------------------------------

select case(typtemps)
 case(time_unsteady) ! corrections de flux seulement en instationnaire

 ! Calcul de l'"energie" a l'interface, en vue de la correction de flux, pour 
 ! le couplage avec echanges espaces
 !DVT : flux%tabscal(1) !
 if (ncp>0) then
   call accumulfluxcorr(dtloc, defsolver, umesh%nboco, umesh%boco, &
                        umesh%nface, flux%tabscal(1)%scal, ncp, &
                        coupling, field, umesh, defsolver%defspat)
 endif

endselect

call delete(flux)


endsubroutine tstep_explicit
!------------------------------------------------------------------------------!
! changes history
!
! avr  2003 : creation de la procedure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
! avr  2004 : decoupage integration_grid -> explicit_step (old name)
! july 2004 : call Navier-Stokes solver integration
! sept 2005 : local time stepping
! Nov  2007 : new name "tstep_explicit"
!------------------------------------------------------------------------------!

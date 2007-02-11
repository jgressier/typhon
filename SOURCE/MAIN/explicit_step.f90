!------------------------------------------------------------------------------!
! Procedure : explicit_step               Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Integration explicit de domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine explicit_step(dtloc, typtemps, defsolver, &
                         umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MENU_ZONECOUPLING
use MATRIX_ARRAY

implicit none

! -- Declaration des entrees --
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(st_ustmesh) :: umesh      ! domaine non structure a integrer
real(krp)        :: dtloc(1:umesh%ncell)         ! pas de temps CFL
integer          :: ncp        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ des valeurs et residus
type(mnu_zonecoupling), dimension(1:ncp) &
                 :: coupling ! donnees de couplage

! -- Declaration des variables internes --
type(st_genericfield) :: flux             ! tableaux des flux
type(st_mattab)       :: jacL, jacR       ! tableaux FICTIFS de jacobiennes des flux

! -- Debut de la procedure --


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
  call erreur("incoherence interne (explicit_step)", "solveur inconnu")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dtloc, umesh, flux, field%residu, .false., jacL, jacR)

! -- calcul pour correction en couplage --

select case(typtemps)
 case(instationnaire) ! corrections de flux seulement en instationnaire

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


endsubroutine explicit_step
!------------------------------------------------------------------------------!
! changes history
!
! avr  2003 : creation de la procedure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
! avr  2004 : decoupage integration_grid -> explicit_step
! july 2004 : call Navier-Stokes solver integration
! sept 2005 : local time stepping
!------------------------------------------------------------------------------!

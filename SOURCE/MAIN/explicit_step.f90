!------------------------------------------------------------------------------!
! Procedure : explicit_step               Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Integration explicit de domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine explicit_step(dt, typtemps, defsolver, defspat, deftime, &
                         umesh, field, coupling, ncp)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrees --
real(krp)        :: dt         ! pas de temps CFL
character        :: typtemps   ! type d'integration (stat, instat, period)
type(mnu_solver) :: defsolver  ! type d'equation a resoudre
type(mnu_spat)   :: defspat    ! parametres d'integration spatiale
type(mnu_time)   :: deftime    ! parametres d'integration spatiale
type(st_ustmesh) :: umesh      ! domaine non structure a integrer
integer          :: ncp        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(st_field)   :: field            ! champ des valeurs et residus
type(mnu_zonecoupling), dimension(1:ncp) &
                 :: coupling ! donnees de couplage

! -- Declaration des variables internes --
type(st_genericfield) :: flux             ! tableaux des flux
real(krp), dimension(1) :: jacL, jacR     ! tableaux FICTIFS de jacobiennes des flux

! -- Debut de la procedure --


! -- allocation des flux et termes sources (structure equivalente a field%etatcons) --

call new(flux, umesh%nface, field%nscal, field%nvect, 0)

! On peut ici decouper le maillage complet en blocs de taille fixe pour optimiser
! l'encombrement memoire et la vectorisation
!print*,'FLUX!'
select case(defsolver%typ_solver)
case(solNS)
  call integration_ns_ust(dt, defsolver, defspat, umesh, field, flux, .false., jacL, jacR)
case(solKDIF)
  call integration_kdif_ust(dt, defsolver, defspat, umesh, field, flux, .false., jacL, jacR)
case default
  call erreur("incoherence interne (explicit_step)", "solveur inconnu")
endselect

! -- flux surfaciques -> flux de surfaces et calcul des residus  --

call flux_to_res(dt, umesh, flux, field%residu, .false., jacL, jacR)

! -- calcul pour correction en couplage --

select case(typtemps)
 case(instationnaire) ! corrections de flux seulement en instationnaire

 ! Calcul de l'"energie" a l'interface, en vue de la correction de flux, pour 
 ! le couplage avec echanges espaces
 !DVT : flux%tabscal(1) !
 if (ncp>0) then
   call accumulfluxcorr(dt, defsolver, umesh%nboco, umesh%boco, &
                        umesh%nface, flux%tabscal(1)%scal, ncp, &
                        coupling)
 endif

endselect

call delete(flux)


endsubroutine explicit_step

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003 : creation de la procedure
! juil 2003 : ajout corrections de  flux
! oct  2003 : corrections de flux seulement en instationnaire
! avr  2004 : changement de nom  integration_ustdomaine -> integration_grid
! avr  2004 : decoupage integration_grid -> explicit_step
! july 2004 : call Navier-Stokes solver integration
!------------------------------------------------------------------------------!

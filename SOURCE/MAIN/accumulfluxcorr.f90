!------------------------------------------------------------------------------!
! Procedure : accumulfluxcorr            Auteur : E.Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!   Accumulation des flux entre deux echanges de donnees entre zone couplees
!   pour correction ulterieure des pertes de flux a l'interface. Orientation
!   selon solver
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine accumulfluxcorr(dt, def_solver, domainenboco, domaineboco, &
                           nface, flux, ncoupling, coupling)
                           

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: def_solver       ! proprietes du solver
integer          :: domainenboco     ! nb de conditions aux limites du domaine
type(st_ustboco), dimension(1:domainenboco) &
                 :: domaineboco      !conditions aux limites du domaine
integer          :: nface            ! nombre de faces du domaine
real(krp), dimension(1:nface) &
                 :: flux
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Declaration des entrees/sorties --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage

! -- Debut de la procedure --

select case(def_solver%typ_solver)
case(solKDIF)
  call accumulfluxcorr_kdif(dt, def_solver%nboco, def_solver%boco, &
                           domainenboco, domaineboco, nface, flux, &
                           ncoupling, coupling)
case default
  call erreur("Incoherence interne (accumulfluxcorr)","type de solveur inconnu")
endselect 

endsubroutine accumulfluxcorr

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

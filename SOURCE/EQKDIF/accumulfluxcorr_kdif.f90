!------------------------------------------------------------------------------!
! Procedure : accumulfluxcorr_kdif        Auteur : E.Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!   Accumulation des flux entre deux echanges de donnees entre zone couplees
!   pour correction ulterieure des pertes de flux a l'interface. Solver de 
!   thermique
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine accumulfluxcorr_kdif(dt, defsolvernboco, defsolverboco, &
                                domainenboco, domaineboco, nface, flux, &
                                ncoupling, coupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrees --
real(krp)        :: dt               ! pas de temps CFL
integer          :: defsolvernboco   ! nb de conditions aux limites du solver
type(mnu_boco), dimension(1:defsolvernboco) &
                 :: defsolverboco    ! conditions aux limites solver
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

! -- Declaration des variables internes --
integer               :: if               ! index de face
integer               :: ib               ! index de conditions aux limites
integer               :: i                ! index de face
integer               :: ic               ! index de couplage
real(krp)             :: rflux, etatcons       

! -- Debut de la procedure --

! Calcul de l'"energie" a l'interface.  On accumule les flux.

! PROVISOIRE : A EFFACER
!do ib =1, domainenboco
!  if (defsolverboco(domaineboco(ib)%idefboco)%typ_boco == bc_coupling) then
!    do i = 1, domaineboco(ib)%nface
!      if = domaineboco(ib)%iface(i)
!      do ic = 1, ncoupling
!        if (samestring(coupling(ic)%family, domaineboco(ib)%family)) then
!          rflux = flux(if)
!          etatcons = coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i)
!          coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i) = etatcons + rflux * dt
!        endif
!      enddo
!    enddo
!  endif
!enddo

do ic =1, ncoupling
  do ib =1, domainenboco
    if (samestring(coupling(ic)%family, domaineboco(ib)%family)) then
      do i = 1, domaineboco(ib)%nface
        if = domaineboco(ib)%iface(i)
        rflux = flux(if)
        etatcons = coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i)
        coupling(ic)%zcoupling%etatcons%tabscal(1)%scal(i) = etatcons + rflux * dt
      enddo
    endif
  enddo
enddo

endsubroutine accumulfluxcorr_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!

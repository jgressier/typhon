!------------------------------------------------------------------------------!
! Procedure : integration_ustdomaine      Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juillet 2003
!   Integration domaine par domaine
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_ustdomaine(dt, defsolver, domaine, field, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
use MENU_ZONECOUPLING

implicit none

! -- Declaration des entrées --
real(krp)        :: dt               ! pas de temps CFL
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(st_ustmesh) :: domaine          ! domaine non structuré à intégrer
integer          :: ncoupling        ! nombre de couplages de la zone

! -- Declaration des entrées/sorties --
type(st_field)   :: field            ! champ des valeurs et résidus
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! données de couplage

! -- Declaration des variables internes --
type(st_genericfield) :: flux             ! tableaux des flux
real(krp)             :: surf             ! surface intermédiaire
integer               :: if               ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables
integer               :: ib               ! index de conditions aux limites
integer               :: i                ! index de face
integer               :: ic               ! index de couplage
real(krp)             :: rajoutflux, rflux, etatcons       

! -- Debut de la procedure --

! -- allocation des flux et termes sources (structure équivalente à field%etatcons) --

call new(flux, domaine%nface, field%nscal, field%nvect, 0)

! A ce niveau, on est censé appeler une routine qui intègre aussi bien les flux
! dans un domaine structuré que dans un domaine non structuré
! On peut ici découper le maillage complet en blocs de taille fixé pour optimiser
! l'encombrement mémoire et la vectorisation

select case(defsolver%typ_solver)
case(solKDIF)
  call integration_kdif_ust(dt, defsolver, domaine, field, flux)
case default
  call erreur("incohérence interne (integration_ustdomaine)", "solveur inconnu")
endselect

! -- flux surfaciques -> flux de surfaces --

do if = 1, domaine%nface
  surf = domaine%mesh%iface(if,1,1)%surface
  do ip = 1, field%nscal
    flux%tabscal(ip)%scal(if) = surf * flux%tabscal(ip)%scal(if)
  enddo
  do ip = 1, field%nvect
    flux%tabvect(ip)%vect(if) = surf * flux%tabvect(ip)%vect(if)
  enddo
enddo

! -- calcul des résidus --

call init_genericfield(field%residu, 0._krp, v3d(0._krp, 0._krp, 0._krp))

! ??? création de procédure intrinsèques ? // optimisation

do if = 1, domaine%nface
  ic1 = domaine%facecell%fils(if,1)
  ic2 = domaine%facecell%fils(if,2)

  do ip = 1, field%nscal
    field%residu%tabscal(ip)%scal(ic1) = field%residu%tabscal(ip)%scal(ic1) - flux%tabscal(ip)%scal(if)
    field%residu%tabscal(ip)%scal(ic2) = field%residu%tabscal(ip)%scal(ic2) + flux%tabscal(ip)%scal(if)
  enddo
  do ip = 1, field%nvect
    field%residu%tabvect(ip)%vect(ic1) = field%residu%tabvect(ip)%vect(ic1) - flux%tabvect(ip)%vect(if)
    field%residu%tabvect(ip)%vect(ic2) = field%residu%tabvect(ip)%vect(ic2) + flux%tabvect(ip)%vect(if)
  enddo
enddo

! ??? création de procédure intrinsèques ? // optmisation

do ic1 = 1, domaine%ncell_int
  do ip = 1, field%nscal
    field%residu%tabscal(ip)%scal(ic1) = dt * field%residu%tabscal(ip)%scal(ic1) &
                                            / domaine%mesh%volume(ic1,1,1)
  enddo
  do ip = 1, field%nvect
    field%residu%tabvect(ip)%vect(ic1) = dt * field%residu%tabvect(ip)%vect(ic1)  &
                                            / domaine%mesh%volume(ic1,1,1)
  enddo
enddo

!!print*,"!! DEBUG-residu :", field%residu%tabscal(1)%scal(1:10)

! Calcul de l'"énergie" à l'interface, en vue de la correction de flux, pour 
! le couplage avec échanges espacés
!DVT : flux%tabscal(1) !
if (ncoupling>0) then
  call accumulfluxcorr(dt, defsolver, domaine%nboco, domaine%boco, &
                       domaine%nface, flux%tabscal(1)%scal, ncoupling, &
                       coupling)
endif

call delete(flux)

endsubroutine integration_ustdomaine

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): création de la procédure
! juillet 2003        : ajout corrections de  flux
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : integration_zone            Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : (cf historique)
!   Intégration de tous les domaines d'une zone sur un pas de temps correspondant 
!   à une itération
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_zone(dt, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE

implicit none

! -- Declaration des entrées --
real(krp)     :: dt              ! pas de temps propre à la zone
type(st_zone) :: zone            ! zone à intégrer

! -- Declaration des sorties --
! retour des résidus à travers le champ field de la structure zone

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
integer                :: if

! -- Debut de la procedure --

! -- Préparation du calcul --

pgrid => zone%grid
do while (associated(pgrid))
  call calc_varprim(zone%defsolver, pgrid%field)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

! -- calcul des conditions aux limites pour tous les domaines --

call conditions_limites(zone)
    
! on ne calcule les gradients que dans les cas nécessaires

if (zone%defspat%calc_grad) then
  pgrid => zone%grid
  do while (associated(pgrid))
    call calc_gradient(zone%defsolver, pgrid%umesh,                 &
                       pgrid%field%etatprim, pgrid%field%gradient)
    call calc_gradient_limite(zone%defsolver, pgrid%umesh, pgrid%field%gradient)
    pgrid => pgrid%next
  enddo
endif

! -- intégration des domaines --

pgrid => zone%grid
do while (associated(pgrid))
  ! DEV : changer les structures de couplages dans MGRID
  call integration_grid(dt, zone%info%typ_temps,                    &
                        zone%defsolver, zone%defspat, zone%deftime, &
                        pgrid, zone%coupling, zone%ncoupling)
  pgrid => pgrid%next
enddo



!-----------------------------
endsubroutine integration_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! août 2002 : création de la procédure
! juil 2003 : modification arguments integration_ustdomaine
! oct  2003  : modification arguments integration_ustdomaine : ajout typ_temps
! oct  2003 : insertion des procédures de calcul var. primitives et gradients
!             (calcul des conditions aux limites avant calcul de gradients)
!             ajout du calcul des gradients aux limites (set_gradient_limite)
! avr  2004 : traitement des listes chaînées de structures MGRID
!             changement d'appel integration_ustdomaine -> integration_grid
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : integration_zone            Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : Juillet 2003
!   Intégration d'une zone sur un pas de temps correspondant 
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
integer     :: ifield

! -- Debut de la procedure --

! -- calcul des conditions aux limites pour tous les domaines --

call conditions_limites(zone)

! -- intégration des domaines --
select case(zone%typ_mesh)

case(mshSTR)
  call erreur("Dévéloppement (integration_zone)", &
              "maillage structuré non implémenté")
  !do i = 1, zone%str_mesh%nblock
  !  call integration_strdomaine(dt, zone%defsolver, zone%str_mesh%block(i))
  !enddo

case(mshUST)
  !call erreur("Développement (integration_zone)", &
  !            "maillage non structuré non implémenté")
  do ifield = 1, zone%ndom
    call integration_ustdomaine(dt, zone%defsolver, zone%ust_mesh, zone%field(ifield),&
                                zone%coupling, zone%ncoupling)
  enddo

case default
  call erreur("incohérence interne (integration_zone)", &
              "type de maillage inconnu")

endselect

!-----------------------------
endsubroutine integration_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! août 2002 (v0.0.1b): création de la procédure
! juillet 2003       : modification arguments integration_ustdomaine
!------------------------------------------------------------------------------!

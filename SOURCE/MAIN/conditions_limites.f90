!------------------------------------------------------------------------------!
! Procedure : conditions_limites          Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : Juin 2003 (cf Historique)
!   Intégration d'une zone sur un écart de temps donné,
!   d'une représentation physique uniquement
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine conditions_limites(lzone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone            ! zone à intégrer

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer      :: ifield            ! indice de champ

! -- Debut de la procedure --
do ifield = 1, lzone%ndom

select case(lzone%typ_mesh)

case(mshSTR)
  call erreur("Dévéloppement (conditions_limites)", &
              "maillage structuré non implémenté")
  !do i = 1, zone%str_mesh%nblock
  !  call integration_strdomaine(dt, zone%defsolver, zone%str_mesh%block(i))
  !enddo

case(mshUST)
  !call erreur("Développement (integration_zone)", &
  !            "maillage non structuré non implémenté")
  call calcboco_ust(lzone%defsolver, lzone%ust_mesh, lzone%field(ifield), &
                    lzone%ncoupling, lzone)

case default
  call erreur("incohérence interne (conditions_limites)", &
              "type de maillage inconnu")

endselect

enddo

endsubroutine conditions_limites

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : init_champ                  Auteur : J. Gressier/ E. Radenac
!                                         Date   : Nov 2003
! Fonction                                Modif  :
!   Lecture des menus
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_boco(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE

implicit none

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone

! -- Declaration des variables internes --
integer :: id             ! index de domaine/champ

! -- Debut de la procedure --

select case(zone%typ_mesh)

case(mshSTR)
  call erreur("Développement (init_boco)", &
              "maillage structuré non implémenté")

case(mshUST)
    call init_boco_ust(zone%defsolver, zone%ust_mesh)

case default
  call erreur("incohérence interne (init_champ)", &
              "type de maillage inconnu")

endselect


endsubroutine init_boco


!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : init_champ                  Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  :
!   Lecture des menus
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_champ(zone)

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

!zone%champ%idim = 1

select case(zone%typ_mesh)

case(mshSTR)
  call erreur("Développement (init_champ)", &
              "maillage structuré non implémenté")

case(mshUST)
  allocate(zone%field(zone%ndom))
  do id = 1, zone%ndom
    call init_champ_ust(zone%defsolver, zone%ust_mesh, zone%field(id))
  enddo

case default
  call erreur("incohérence interne (init_champ)", &
              "type de maillage inconnu")

endselect


endsubroutine init_champ


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!

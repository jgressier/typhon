!------------------------------------------------------------------------------!
! Procedure : init_champ                  Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
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


allocate(zone%field(zone%ndom))
do id = 1, zone%ndom
  call init_champ_ust(zone%defsolver, zone%ust_mesh, zone%field(id))
enddo



endsubroutine init_champ


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : création de la procédure
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : init_champ                  Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_champ(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE

implicit none

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid 
integer                :: id  

! -- Debut de la procedure --

!zone%champ%idim = 1

pgrid => zone%grid

do while (associated(pgrid))
  print*,"init field"
  allocate(pgrid%field)
  !call init_champ_ust(zone%defsolver, pgrid%umesh, pgrid%field, pgrid)
  call init_champ_ust(zone%defsolver, pgrid%umesh, pgrid)
  print*,"end init field"
  !allocate(pgrid%info%field_loc)
  pgrid%info%field_loc => pgrid%field
  pgrid => pgrid%next
enddo

endsubroutine init_champ
!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : creation de la procedure
! avr  2004 : application a liste chainee de MGRID
! oct  2004 : field chained list
!------------------------------------------------------------------------------!

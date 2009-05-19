!------------------------------------------------------------------------------!
! Procedure : init_champ                  Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des champs
!
!------------------------------------------------------------------------------!
subroutine init_champ(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE

implicit none

! -- INPUTS --

! -- OUTPUTS --

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
type(st_grid), pointer :: pgrid 
integer                :: id  

! -- BODY --

!zone%champ%idim = 1

pgrid => zone%gridlist%first

do while (associated(pgrid))
  allocate(pgrid%field)

  call init_gridfield_ust(zone%defsolver, pgrid%umesh, pgrid)

  pgrid%info%field_loc => pgrid%field
  pgrid => pgrid%next
enddo

endsubroutine init_champ
!------------------------------------------------------------------------------!
! Changes history
!
! mars 2003 : creation de la procedure
! avr  2004 : application a liste chainee de MGRID
! oct  2004 : field chained list
!------------------------------------------------------------------------------!

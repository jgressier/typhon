!------------------------------------------------------------------------------!
! Procedure : def_check                                Authors : J. Gressier
!                                                     Created : March 2003
! Fonction 
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
!------------------------------------------------------------------------------!
subroutine def_check(zone)

use TYPHMAKE
use VARCOM
use DEFZONE

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --


! -- BODY --

call print_info(5,"- Check parameters consistency and further initialization")

! -------------------------------------------------
! define mesh splitting parameters

select case(zone%defmesh%splitmesh)
case(split_none)
case(split_svm2quad)
  call print_info(10, "  initialize SVM mesh splitting parameters")
  call init_svmmesh(zone%defmesh%splitmesh, zone%defmesh%svm)
case default
  call erreur("Development", "unknown mesh splitting parameter (def_check)")
endselect

endsubroutine def_check

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2007: creation
!------------------------------------------------------------------------------!



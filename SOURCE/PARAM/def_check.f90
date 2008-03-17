!------------------------------------------------------------------------------!
! Procedure : def_check     
!                           
! Fonction 
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
  zone%defsolver%defspat%svm%sv_meth = svm_2quad
  call print_info(10, "  initialize SVM parameters")
  call init_svmparam(zone%defsolver%defspat%svm)
case default
  call erreur("Development", "unknown mesh splitting parameter (def_check)")
endselect


endsubroutine def_check

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2007: creation
! Mar  2008: SVM parameters
!------------------------------------------------------------------------------!



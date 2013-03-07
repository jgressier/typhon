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
! define mesh splitting parameters (according to MESH or SVM definition)

call init_splitparam(zone%defsolver%defmesh%defsplit)

zone%defsolver%defmesh%nfgauss = max(zone%defsolver%defmesh%nfgauss, zone%defsolver%defspat%svm%nfgauss)


endsubroutine def_check

!------------------------------------------------------------------------------!
! Changes history
!
! Nov  2007: creation
! Mar  2008: SVM parameters
! Mar  2013: check splitting params
!------------------------------------------------------------------------------!



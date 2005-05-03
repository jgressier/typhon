!------------------------------------------------------------------------------!
! Procedure : analyse                             Authors : J. Gressier
!                                                 Created : May 2005
! Fonction                                        Modif   : (cf history)
!   Analyse project and report information
!
!------------------------------------------------------------------------------!
subroutine analyse(lworld)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees/sorties --
type(st_world) :: lworld

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_grid), pointer :: pgrid
!real(krp)             :: macro_dt
integer, dimension(:), allocatable &
                       :: exchcycle ! indices des cycles d'echange pour les differents couplages de zones
integer                :: ir, izone, if, ib, ic
integer                :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- Debut de la procedure --


!--------------------------------------------------------
! ANALYSE
!--------------------------------------------------------
do izone = 1, lworld%prj%nzone
  
  call analyse_zone(lworld%zone(izone))

  !call print_info(6,str_w)

enddo



endsubroutine analyse

!------------------------------------------------------------------------------!
! Change history
!
! may  2005 : creation (loop to analyse zones)
!------------------------------------------------------------------------------!

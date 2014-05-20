!------------------------------------------------------------------------------!
! Procedure : calc_wallvelocity           Author : A. Gardi
!                                         Date   : Apr 2011
!> @brief Boundary conditions update to account for a moving wall in ALE and MRF analyses
!------------------------------------------------------------------------------!
subroutine calc_wallvelocity(defale, defmrf, wall_velocity, face_center, indf, curtime)

use TYPHMAKE
use MENU_ALE
use MESHMRF
use MESHBASE
use OUTPUT
use VARCOM

implicit none

! -- INPUTS --
type(mnu_ale)          :: defale        ! ALE parametres
type(mnu_mrf)          :: defmrf        ! MRF parametres
type(v3d)              :: face_center
integer                :: indf
real(krp)              :: curtime

! -- INPUT/OUTPUT --
type(v3d)              :: wall_velocity    ! fields

! -- Internal variables --

! -- BODY --

! MRF wall movement contributes
if (defmrf%type /= mrf_none .and. defmrf%input == mrfdata_absolute) then
  call mrfvel_abs2rel(defmrf, curtime, face_center, wall_velocity)
endif

! ALE wall movement contributes
if ((defale%type /= ale_none) .and. associated(defale%face_velocity)) then
  wall_velocity = wall_velocity + defale%face_velocity(indf)
endif

endsubroutine calc_wallvelocity

!------------------------------------------------------------------------------!
! Changes history
!
! apr  2011: creation
!------------------------------------------------------------------------------!

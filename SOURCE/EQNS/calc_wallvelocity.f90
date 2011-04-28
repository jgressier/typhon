!------------------------------------------------------------------------------!
! Procedure : calc_wallvelocity           Author : A. Gardi
!                                         Date   : Apr 2011
! Fonction                                Modif  : (cf Historique)
!   Boundary conditions update to account for a moving wall in ALE and MRF
!   analyses
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_wallvelocity(defale, defmrf, wall_velocity, iface, indf, curtime)

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
type(st_face)          :: iface
integer                :: indf
real(krp)              :: curtime

! -- INPUT/OUTPUT --
type(v3d)              :: wall_velocity    ! fields

! -- Internal variables --

! -- BODY --

! MRF wall movement contributes
if (defmrf%type /= mrf_none) then
  call mrfvel_abs2rel(defmrf, curtime, iface%centre, wall_velocity)
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

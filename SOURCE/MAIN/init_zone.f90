!------------------------------------------------------------------------------!
! Procedure : init_zone               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des zone
!     - zone par defaut
!     - zone defnis par l'utilisateur
!
!------------------------------------------------------------------------------!
subroutine init_zone(zone, prj)

use TYPHMAKE
use DEFZONE
use MENU_GEN

implicit none

! -- Inputs --
type(mnu_project) :: prj

! -- Inputs/Outputs --
type(st_zone) :: zone

! -- BODY --

zone%info%time_model =  prj%time_model

select case(zone%info%time_model)

case(time_steady)
  zone%info%mon_it     = .true.
  zone%info%mon_time   = .false.
  zone%info%mon_res    = .true.
  zone%info%mon_cflmax = .true.
  zone%info%mon_dtmin  = .true.

case(time_unsteady, time_unsteady_inverse)
  zone%info%mon_it     = .true.
  zone%info%mon_time   = .true.
  zone%info%mon_res    = .false.
  zone%info%mon_cflmax = .true.
  zone%info%mon_dtmin  = .true.

case default
   call error_stop("internal error (init_zone): unknown time model")
endselect

endsubroutine init_zone
!------------------------------------------------------------------------------!
! Changes history
!
! aug  2004: creation
! Apr  2014: outputs
!------------------------------------------------------------------------------!

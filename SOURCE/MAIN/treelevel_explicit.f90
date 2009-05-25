!------------------------------------------------------------------------------!
! Procedure : treelevel_explicit                    Auteur : J. Gressier
!                                                Date   : March 2006
! Fonction                                       Modif  : (cf history)
!   Time Integration during one timestep of ONE LEVEL of UST grid TREE structure
!
!------------------------------------------------------------------------------!
subroutine treelevel_explicit(dt, info, defsolver, gridlist, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use MODINFO
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling        ! nombre de couplages de la zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage
! retour des residus a travers le champ field de la structure zone

! -- Internal variables --

! -- BODY --

call calc_rhs(dt, info, defsolver, gridlist, coupling, ncoupling)


!-----------------------------
endsubroutine treelevel_explicit

!------------------------------------------------------------------------------!
! Changes history
!
! May  2009: transfer treelevel_explicit to calc_rhs, replaced by call
!------------------------------------------------------------------------------!

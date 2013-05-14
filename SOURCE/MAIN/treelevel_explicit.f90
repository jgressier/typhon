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
!GMRES_FREE!use SPARSE_MAT

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling       ! nombre de couplages de la zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage
! retour des residus a travers le champ field de la structure zone

! -- Internal variables --

! -- Body --

call calc_rhs(dt, info, defsolver, gridlist, coupling, ncoupling)

!GMRES_FREE!select case(defsolver%deftime%tps_meth)
!GMRES_FREE!
!GMRES_FREE!!case(tps_expl, tps_rk2, tps_rk2ssp, tps_rk3ssp, tps_rk4)
!GMRES_FREE!
!GMRES_FREE!case(tps_impl, tps_dualt)
!GMRES_FREE!
!GMRES_FREE!  if (defsolver%deftime%implicite%storage == mat_none) &
!GMRES_FREE!call print_info(-1,"Appel GMRES-FREE")
!GMRES_FREE!call flush(6)
!GMRES_FREE!  if (defsolver%deftime%implicite%storage == mat_none) &
!GMRES_FREE!    call gmres_free(dt, info, defsolver, gridlist, coupling, ncoupling)
!GMRES_FREE!
!GMRES_FREE!endselect


!-----------------------------
endsubroutine treelevel_explicit

!------------------------------------------------------------------------------!
! Changes history
!
! May 2009: transfer treelevel_explicit to calc_rhs, replaced by call
!------------------------------------------------------------------------------!

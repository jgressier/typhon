!------------------------------------------------------------------------------!
! Procedure : calc_visc_suther               Authors : J. Gressier - E. Radenac
!                                                Created :June 2005
! Fonction                                       Modif   : (cf history)
!   Computation of dynamic viscosity using Sutherland's formula
!
!------------------------------------------------------------------------------!
subroutine calc_visc_suther(defns, nf, temp, mu, specie)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D

implicit none

! -- Declaration des entrees --
type(mnu_ns)          :: defns            ! definition parameters of NS solver
integer               :: nf               ! number of parsed faces
real(krp), dimension(1:nf) &
                      :: temp             ! temperature
integer               :: specie           ! specie index

! -- Declaration des sorties --

real(krp), dimension(1:nf) &
                      :: mu               ! dynamic viscosity

! -- Declaration des variables internes --
integer               :: if
real(krp)             :: mu0, T0, S

! -- Debut de la procedure --

mu0 = defns%properties(specie)%visc_dyn ! dynamic viscosity at 
                                                  ! reference temperature 
T0 = defns%properties(specie)%tref      ! reference temperature
S = defns%properties(specie)%tsuth      ! Sutherland's constant

do if = 1, nf
  mu(if) = mu0 * (temp(if)/T0)**(3/2)*(T0+S)/(temp(if)+S)
enddo

endsubroutine calc_visc_suther

!------------------------------------------------------------------------------!
! Changes history
!
! Jun  2005 : creation
!------------------------------------------------------------------------------!

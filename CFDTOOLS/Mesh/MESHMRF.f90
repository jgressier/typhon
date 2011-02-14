!------------------------------------------------------------------------------!
! MODULE : MESHMRF
!
! Fonction
!   Definition of MOVING REFERENCE FRAME parameters
!
!------------------------------------------------------------------------------!
module MESHMRF

use VEC3D
use IOCFD

implicit none

! -- Global variables -------------------------------------------

! -- type of MRF definition --

integer(kpp), parameter :: mrf_none      = 0
integer(kpp), parameter :: mrf_trans_cst = 1
integer(kpp), parameter :: mrf_trans_lin = 5
integer(kpp), parameter :: mrf_trans_osc = 10
integer(kpp), parameter :: mrf_rot_cst   = 20
integer(kpp), parameter :: mrf_rot_lin   = 21
integer(kpp), parameter :: mrf_rot_osc   = 31

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_MRF : Moving reference frame parameters
!------------------------------------------------------------------------------!
type mnu_mrf
  character(len=shortname) :: name
  integer(kpp)             :: type
  type(v3d)          :: center        ! position of center (for rotation)
  type(v3d)          :: velocity      ! center (initial) velocity
  type(v3d)          :: acceleration  ! center acceleration
  type(v3d)          :: axis          ! rotation axis
  real(krp)          :: omega, osc_period, osc_angle
endtype mnu_mrf



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! absolute to relative velocity transformation
!------------------------------------------------------------------------------!
subroutine  mrf_abs2rel(mrf, time, pos, velocity)
implicit none
! -- INPUTS --
type(mnu_mrf)    :: mrf
real(krp)        :: time
type(v3d)        :: pos
! INPUT/OUTPUT
type(v3d)        :: velocity
! -- INTERNAL VARIABLES --
real(krp)          :: theta, osc_om
type(v3d)          :: omega, radius, mrfvelocity

! -- BODY --

! Angular parameters
osc_om = 2._krp*acos(-1._krp)/mrf%osc_period   ! DEV: must use MATH module in future
theta =  mrf%omega*time    + mrf%osc_angle * sin(osc_om*time)
omega = (mrf%omega  + osc_om*mrf%osc_angle * cos(osc_om*time)) * mrf%axis

select case(mrf%type)
case(mrf_none)
case(mrf_rot_osc)
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity - mrfvelocity
case default
  call cfd_error("(MRF) unknown or not implemented MRF definition (mrf_abs2rel)")
endselect

endsubroutine mrf_abs2rel


endmodule MESHMRF
!------------------------------------------------------------------------------!
! Changes history
!
! Jan  2011 : created, A. Gardi development integration
!------------------------------------------------------------------------------!




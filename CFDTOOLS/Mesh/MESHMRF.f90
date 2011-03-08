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
  real(krp)          :: omega, osc_period, osc_angle, phi
endtype mnu_mrf



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! absolute to relative velocity transformation
!------------------------------------------------------------------------------!
subroutine  mrfvel_abs2rel(mrf, time, pos, velocity)
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

select case(mrf%type)
case(mrf_none)

case(mrf_rot_cst)
  theta =  mrf%omega * time
  omega =  mrf%omega * mrf%axis
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity - mrfvelocity

case(mrf_rot_osc)
  osc_om = 2._krp*acos(-1._krp)/mrf%osc_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%osc_angle * sin(osc_om*time + mrf%phi)
  omega = (mrf%omega  + osc_om*mrf%osc_angle * cos(osc_om*time + mrf%phi)) * mrf%axis
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity - mrfvelocity

case default
  call cfd_error("(MRF) unknown or not implemented MRF definition (mrfvel_abs2rel)")
endselect

endsubroutine mrfvel_abs2rel

!------------------------------------------------------------------------------!
! relative to absolute velocity transformation
!------------------------------------------------------------------------------!
subroutine  mrfvel_rel2abs(mrf, time, pos, velocity)
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
select case(mrf%type)
case(mrf_none)

case(mrf_rot_cst)
  theta =  mrf%omega * time
  omega =  mrf%omega * mrf%axis
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity + mrfvelocity
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%axis, theta)

case(mrf_rot_osc)
  osc_om = 2._krp*acos(-1._krp)/mrf%osc_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%osc_angle * sin(osc_om*time + mrf%phi)
  omega = (mrf%omega  + osc_om*mrf%osc_angle * cos(osc_om*time + mrf%phi)) * mrf%axis
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity + mrfvelocity
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%axis, theta)

case default
  call cfd_error("(MRF) unknown or not implemented MRF definition (mrfvel_rel2abs)")
endselect

endsubroutine mrfvel_rel2abs



!------------------------------------------------------------------------------!
! mesh movement due to MRF, for absolute output purposes
!------------------------------------------------------------------------------!
subroutine mrfpos_rel2abs(mrf, time, vertex)
implicit none

! -- INPUTS --
type(mnu_mrf)    :: mrf
real(krp)        :: time

! INPUT/OUTPUT
type(v3d), intent(inout) :: vertex

! -- INTERNAL VARIABLES --
real(krp)          :: theta, osc_om
type(v3d)          :: radius

select case(mrf%type)
case(mrf_none)

case(mrf_rot_cst)
  theta   =  mrf%omega * time
  radius  =  vertex - mrf%center
  call rot(radius, mrf%axis, theta)
  vertex  =  mrf%center + radius

case(mrf_rot_osc)
  osc_om  = 2._krp*acos(-1._krp)/mrf%osc_period   ! DEV: must use MATH module in future
  theta   =  mrf%omega*time    + mrf%osc_angle * sin(osc_om*time + mrf%phi)
  radius  =  vertex - mrf%center
  call rot(radius, mrf%axis, theta)
  vertex  =  mrf%center + radius

case default
  call cfd_error("(MRF) unknown or not implemented MRF definition (mrfpos_rel2abs)")
endselect

endsubroutine mrfpos_rel2abs


endmodule MESHMRF
!------------------------------------------------------------------------------!
! Changes history
!
! Jan  2011 : created, A. Gardi development integration
! Feb  2011 : addition of mrfvel_rel2abs and mrfpos_rel2abs
!------------------------------------------------------------------------------!

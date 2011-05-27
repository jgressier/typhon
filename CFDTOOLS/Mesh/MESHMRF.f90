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
integer(kpp), parameter :: mrf_trans_lin = 5
integer(kpp), parameter :: mrf_trans_osc = 10
integer(kpp), parameter :: mrf_rot_cst   = 11
integer(kpp), parameter :: mrf_rot_osc   = 21
integer(kpp), parameter :: mrf_comb_osc  = 31

! -- type of MRF DATA  --

integer(kpp), parameter :: mrfdata_absolute = 10
integer(kpp), parameter :: mrfdata_relative = 11



! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_MRF : Moving reference frame parameters
!------------------------------------------------------------------------------!
type mnu_mrf
  character(len=shortname) :: name
  integer(kpp)             :: type, input, output
  type(v3d)          :: center        ! position of center (for rotation)
  type(v3d)          :: velocity      ! center (initial) velocity
  type(v3d)          :: acceleration  ! center acceleration
  type(v3d)          :: rot_axis      ! rotation axis
  type(v3d)          :: trn_dir       ! translational oscillation direction
  real(krp)          :: omega, rot_period, rot_ampl, rot_phi, &
                               trn_period, trn_ampl, trn_phi
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
real(krp)          :: theta, rot_om, trn_om
type(v3d)          :: omega, radius, mrfvelocity

! -- BODY --

! Angular parameters

select case(mrf%type)
case(mrf_none)

case(mrf_trans_lin)
  mrfvelocity = mrf%velocity + time * mrf%acceleration
  velocity = velocity - mrfvelocity

case(mrf_trans_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  mrfvelocity = mrf%velocity + time * mrf%acceleration  &
                + trn_om * mrf%trn_ampl * cos(trn_om*time + mrf%trn_phi) * mrf%trn_dir
  velocity = velocity - mrfvelocity

case(mrf_rot_cst)
  theta =  mrf%omega * time
  omega =  mrf%omega * mrf%rot_axis
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity - mrfvelocity

case(mrf_rot_osc)
  rot_om = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  omega = (mrf%omega  + rot_om*mrf%rot_ampl * cos(rot_om*time + mrf%rot_phi)) * mrf%rot_axis
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity - mrfvelocity

case(mrf_comb_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  rot_om = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  omega = (mrf%omega  + rot_om*mrf%rot_ampl * cos(rot_om*time + mrf%rot_phi)) * mrf%rot_axis
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, -theta)
  ! Vabs -> Vrel thanks to MRF velocity (linear translation, omega(t) x r, oscillating translation)
  radius      =  pos - mrf%center
  mrfvelocity = mrf%velocity + time * mrf%acceleration + omega .vect. radius  &
                + trn_om * mrf%trn_ampl * cos(trn_om*time + mrf%trn_phi) * mrf%trn_dir
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
real(krp)          :: theta, rot_om, trn_om
type(v3d)          :: omega, radius, mrfvelocity

! -- BODY --
select case(mrf%type)
case(mrf_none)

case(mrf_trans_lin)
  mrfvelocity = mrf%velocity + time * mrf%acceleration
  velocity = velocity + mrfvelocity

case(mrf_trans_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  mrfvelocity = mrf%velocity + time * mrf%acceleration  &
                + trn_om * mrf%trn_ampl * cos(trn_om*time + mrf%trn_phi) * mrf%trn_dir
  velocity = velocity + mrfvelocity

case(mrf_rot_cst)
  theta =  mrf%omega * time
  omega =  mrf%omega * mrf%rot_axis
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity + mrfvelocity
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, theta)

case(mrf_rot_osc)
  rot_om = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  omega = (mrf%omega  + rot_om*mrf%rot_ampl * cos(rot_om*time + mrf%rot_phi)) * mrf%rot_axis
  ! Vabs -> Vrel thanks to MRF velocity (omega(t) x r)
  radius      =  pos - mrf%center
  mrfvelocity = omega .vect. radius
  velocity    = velocity + mrfvelocity
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, theta)

case(mrf_comb_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  rot_om = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  omega = (mrf%omega  + rot_om*mrf%rot_ampl * cos(rot_om*time + mrf%rot_phi)) * mrf%rot_axis
  ! Vabs -> Vrel thanks to MRF velocity (linear translation, omega(t) x r, oscillating translation)
  radius      =  pos - mrf%center
  mrfvelocity = mrf%velocity + time * mrf%acceleration + omega .vect. radius  &
                + trn_om * mrf%trn_ampl * cos(trn_om*time + mrf%trn_phi) * mrf%trn_dir
  velocity    = velocity + mrfvelocity
  ! Boundary Vabs rotation due to MRF position (theta(t))
  call rot(velocity, mrf%rot_axis, theta)

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
real(krp)          :: theta, rot_om, trn_om
type(v3d)          :: radius, mrfdisplacement

select case(mrf%type)
case(mrf_none)

case(mrf_trans_lin)
  mrfdisplacement = time * mrf%velocity + (0.5_krp * time**2 * mrf%acceleration)
  vertex = vertex + mrfdisplacement

case(mrf_trans_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  mrfdisplacement = time * mrf%velocity + (0.5_krp * time**2 * mrf%acceleration)  &
                + mrf%trn_ampl * sin(trn_om*time + mrf%trn_phi) * mrf%trn_dir
  vertex = vertex + mrfdisplacement

case(mrf_rot_cst)
  theta   =  mrf%omega * time
  radius  =  vertex - mrf%center
  call rot(radius, mrf%rot_axis, theta)
  vertex  =  mrf%center + radius

case(mrf_rot_osc)
  rot_om  = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta   =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  radius  =  vertex - mrf%center
  call rot(radius, mrf%rot_axis, theta)
  vertex  =  mrf%center + radius

case(mrf_comb_osc)
  trn_om = 2._krp*acos(-1._krp)/mrf%trn_period   ! DEV: must use MATH module in future
  rot_om  = 2._krp*acos(-1._krp)/mrf%rot_period   ! DEV: must use MATH module in future
  theta   =  mrf%omega*time    + mrf%rot_ampl * sin(rot_om*time + mrf%rot_phi)
  radius  =  vertex - mrf%center
  call rot(radius, mrf%rot_axis, theta)
  mrfdisplacement = time * mrf%velocity + (0.5_krp * time**2 * mrf%acceleration)  &
                + mrf%trn_ampl * sin(trn_om*time + mrf%trn_phi) * mrf%trn_dir
  vertex  =  mrf%center + radius + mrfdisplacement

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
! May  2011 : addition of mrf_trans_osc and other missing stuff, development of
!             mrf_comb_osc (linear AND oscillating rotation AND translation) and global revision
!------------------------------------------------------------------------------!

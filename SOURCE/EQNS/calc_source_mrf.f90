!------------------------------------------------------------------------------!
! Procedure : calc_sources_mrf				Authors : A.Gardi
!							Date   : Dec 2010
! Function
!   Computation of source terms due to non-inertially Moving Reference Frame
!	for NS equations
!
!------------------------------------------------------------------------------!
subroutine calc_source_mrf(umesh, field, mrf, curtime)

use TYPHMAKE
use VARCOM
use OUTPUT
use USTMESH
use DEFFIELD
use EQNS
use MENU_SOLVER
use MESHMRF
use MATH

implicit none

! INPUTS
type(st_ustmesh)        :: umesh
type(mnu_mrf)           :: mrf
real(krp)               :: curtime

! INPUTS/OUTPUTS
type(st_field) :: field

! INTERNAL
integer   :: ic	! index on cells
real(krp) :: rho, mass
real(krp) :: rot_om, trn_om
type(v3d) :: speed, radius, omega, dotomega

type(v3d) :: centrifugal, coriolis, linacc, rotacc
type(v3d) :: deltamomentum
real(krp) :: deltaenergy

! -- BODY --

select case(mrf%type)

 case(mrf_none)
  ! nothing to do

 case(mrf_trans_lin)

  do ic = 1, umesh%ncell_int
    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    deltamomentum = -mass*mrf%acceleration
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + deltamomentum
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + (speed .scal. deltamomentum)
  enddo

 case(mrf_trans_osc)

  trn_om   = 2._krp*pi/mrf%trn_period
  do ic = 1, umesh%ncell_int
    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    deltamomentum = -mass*(mrf%acceleration - trn_om**2 * mrf%trn_ampl * sin(trn_om * curtime + mrf%trn_phi) * mrf%trn_dir)
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + deltamomentum
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + (speed .scal. deltamomentum)
  enddo

 case(mrf_rot_cst)

  omega = mrf%omega * mrf%rot_axis

  do ic = 1, umesh%ncell_int
    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    radius = umesh%mesh%centre(ic,1,1) - mrf%center

    centrifugal = omega .vect. (omega .vect. radius)
    coriolis    = 2._krp * (omega .vect. speed)

    deltamomentum = -mass*(centrifugal+coriolis)
    deltaenergy = speed.scal.deltamomentum
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + deltamomentum
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + deltaenergy
  enddo

 case(mrf_rot_osc)

  rot_om   = 2._krp*pi/mrf%rot_period
  omega    = (mrf%omega + rot_om * mrf%rot_ampl * cos(rot_om*curtime + mrf%rot_phi) )*mrf%rot_axis
  dotomega = (        -rot_om**2 * mrf%rot_ampl * sin(rot_om*curtime + mrf%rot_phi) )*mrf%rot_axis

  do ic = 1, umesh%ncell_int

    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    radius = umesh%mesh%centre(ic,1,1) - mrf%center

    centrifugal = omega .vect. (omega .vect. radius)
    coriolis    = 2._krp * (omega .vect. speed)
    rotacc      = dotomega .vect. radius

    deltamomentum = -mass*(centrifugal+coriolis+rotacc)
    !deltaenergy = mass*(-(speed.scal.omega)*(radius.scal.omega) + (speed.scal.radius)*abs(omega)**2 -(speed.scal.rotacc)) NON-CONSERVATIVE?
    deltaenergy = speed.scal.deltamomentum
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + deltamomentum
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + deltaenergy
  enddo

 case(mrf_comb_osc)

  trn_om   = 2._krp*pi/mrf%trn_period
  rot_om   = 2._krp*pi/mrf%rot_period
  omega    = (mrf%omega + rot_om * mrf%rot_ampl * cos(rot_om*curtime + mrf%rot_phi) )*mrf%rot_axis
  dotomega = (        -rot_om**2 * mrf%rot_ampl * sin(rot_om*curtime + mrf%rot_phi) )*mrf%rot_axis

  do ic = 1, umesh%ncell_int

    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    radius = umesh%mesh%centre(ic,1,1) - mrf%center
  
    linacc      = mrf%acceleration - trn_om**2 * mrf%trn_ampl * sin(trn_om * curtime + mrf%trn_phi) * mrf%trn_dir
    centrifugal = omega .vect. (omega .vect. radius)
    coriolis    = 2._krp * (omega .vect. speed)
    rotacc      = dotomega .vect. radius

    deltamomentum = -mass*(linacc+centrifugal+coriolis+rotacc)
    deltaenergy = speed.scal.deltamomentum
    field%residu%tabvect(1)%vect(ic) = field%residu%tabvect(1)%vect(ic) + deltamomentum
    field%residu%tabscal(2)%scal(ic) = field%residu%tabscal(2)%scal(ic) + deltaenergy
  enddo

 case default
    call error_stop("unknown or not implement MRF source term")

endselect

end subroutine calc_source_mrf
!------------------------------------------------------------------------------!
! Changes history
!
! Jan  2011 : creation, MRF source terms(A. Gardi & JG)
! May  2011 : addition of missing cases (mrf_trans_osc, mrf_comb_osc) and global revision
!------------------------------------------------------------------------------!

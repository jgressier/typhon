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
real(krp) :: om_osc
type(v3d) :: speed
type(v3d) :: radius
type(v3d) :: omega
type(v3d) :: dotomega

type(v3d) :: centrifugal
type(v3d) :: coriolis
type(v3d) :: rotacc
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

 case(mrf_rot_cst)

  omega = mrf%omega * mrf%axis

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

  om_osc   = 2._krp*pi/mrf%osc_period
  omega    = (mrf%omega + om_osc * mrf%osc_angle * cos(om_osc*curtime + mrf%phi) )*mrf%axis
  dotomega = (        -om_osc**2 * mrf%osc_angle * sin(om_osc*curtime + mrf%phi) )*mrf%axis

  do ic = 1, umesh%ncell_int

    rho    = field%etatcons%tabscal(1)%scal(ic)
    speed  = field%etatcons%tabvect(1)%vect(ic) / rho
    mass   = umesh%mesh%volume(ic,1,1) * rho
    radius = umesh%mesh%centre(ic,1,1) - mrf%center
  
    centrifugal = omega .vect. (omega .vect. radius)
    coriolis    = 2._krp * (omega .vect. speed)
    rotacc      = dotomega .vect. radius

    !deltamomentum = mass*(-mrf%linacc-centrifugal-coriolis-rotacc)
    deltamomentum = -mass*(centrifugal+coriolis+rotacc)
    !deltaenergy = mass*(-(speed.scal.omega)*(radius.scal.omega) + (speed.scal.radius)*abs(omega)**2 -(speed.scal.rotacc)) NON-CONSERVATIVE?
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
! jan  2010 : creation, MRF source terms(A. Gardi & JG)
!------------------------------------------------------------------------------!

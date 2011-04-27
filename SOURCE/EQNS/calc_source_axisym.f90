!------------------------------------------------------------------------------!
! Procedure : calc_sources_axisym			Authors : G.Lavalle
!							Date    : Apr 2011
! Function
!   Computation of axisymmetric solver for Euler equations
!
!------------------------------------------------------------------------------!
subroutine calc_source_axisym(umesh, field, defns, curtime)

use PACKET
use OUTPUT
use USTMESH
use DEFFIELD
use EQNS
use MENU_NS
use FCT_EVAL

implicit none

! INPUTS
type(st_ustmesh)        :: umesh
type(mnu_ns)            :: defns
real(krp)               :: curtime

! INPUTS/OUTPUTS
type(st_field) :: field

! INTERNAL
integer   :: ic	                                         ! index on cells
real(krp) :: rho, pres, vely, velz, volume
integer, pointer      :: ista(:), iend(:)                ! starting and ending index
integer               :: ib, buf, nblock                 ! buffer size 

! -- BODY --

select case(defns%typ_fluid)
case(eqEULERaxi, eqNSLAMaxi)
  call new_buf_index(umesh%ncell_int, cell_buffer, nblock, ista, iend)
  !$OMP PARALLEL DO private(ic, buf, volume, rho, vely, velz, pres) shared(ista, iend)
  do ib = 1, nblock
    buf = iend(ib)-ista(ib)+1
    do ic = ista(ib), iend(ib)
      volume  = umesh%mesh%volume(ic,1,1) / umesh%mesh%centre(ic,1,1)%y   ! 2D volume
      rho     = field%etatcons%tabscal(1)%scal(ic)
      vely    = field%etatprim%tabvect(1)%vect(ic)%y
      velz    = field%etatprim%tabvect(1)%vect(ic)%z
      pres    = field%etatprim%tabscal(2)%scal(ic)
      !field%residu%tabscal(1)%scal(ic)   = field%residu%tabscal(1)%scal(ic)  ! nothing to do
      !field%residu%tabscal(2)%scal(ic)   = field%residu%tabscal(2)%scal(ic)  ! nothing to do
      field%residu%tabvect(1)%vect(ic)%x = field%residu%tabvect(1)%vect(ic)%x
      field%residu%tabvect(1)%vect(ic)%y = field%residu%tabvect(1)%vect(ic)%y + (rho * velz**2 + pres) * volume
      field%residu%tabvect(1)%vect(ic)%z = field%residu%tabvect(1)%vect(ic)%z - (rho * vely * velz)    * volume
     enddo
  enddo
  !$OMP END PARALLEL DO
  deallocate(ista, iend)

case(eqEULER, eqNSLAM, eqRANS, eqNSLES)
  ! nothing to do
case default
  call error_stop("axi source terms computation: unknown model")
endselect

end subroutine calc_source_axisym
!------------------------------------------------------------------------------!
! Changes history
!
! apr  2011 : creation, axisymmetric source terms
!------------------------------------------------------------------------------!

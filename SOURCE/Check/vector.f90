program testvector

use TYPHMAKE
use GEO3D

implicit none

integer i, n
real(krp) :: itime, time, dt
real(krp), dimension(:),   allocatable :: r
type(v3d), dimension(:),   allocatable :: tv1, tv2
real(krp), dimension(:,:), allocatable :: xv1, xv2

print*,"..... test vector implementation"

n = 1000000
allocate(r(n))
allocate(tv1(n))
allocate(tv2(n))

do i = 1, n
  tv1(i)    = v3d(real(i,krp),    2*real(i,krp), 1./real(i,krp) )
  tv2(i)    = v3d(3./real(i,krp), 3*real(i,krp), 1./real(i,krp) )
enddo

!----------------------------------------------------------
call cpu_time(itime)
r(1:n) = tv1.scal.tv2
call cpu_time(time) ; dt = time-itime
print*,"V3D     .scal. function   ",dt

!----------------------------------------------------------
call cpu_time(itime)
do i = 1, n
  r(i) = tv1(i)%x*tv2(i)%x + tv1(i)%y*tv2(i)%y + tv1(i)%z*tv2(i)%z
enddo
call cpu_time(time) ; dt = time-itime
print*,"V3D     .scal. inline     ",dt

deallocate(tv1)
deallocate(tv2)

allocate(xv1(n,3))
allocate(xv2(n,3))

do i = 1, n
  xv1(i, 1:3) = (/ real(i,krp),    2*real(i,krp), 1./real(i,krp) /)
  xv2(i, 1:3) = (/ 3./real(i,krp), 3*real(i,krp), 1./real(i,krp) /)
enddo

!----------------------------------------------------------
call cpu_time(itime)
do i = 1, n
  r(i) = xv1(i,1)*xv2(i,1) + xv1(i,2)*xv2(i,2) + xv1(i,3)*xv2(i,3)
enddo
call cpu_time(time) ; dt = time-itime
print*,"v(i,1:3) .scal. inline    ",dt

!----------------------------------------------------------
call cpu_time(itime)
r(1:n) = sum(xv1*xv2, dim=2)
call cpu_time(time) ; dt = time-itime
print*,"v(i,1:3) .scal. vectorize ",dt

deallocate(xv1)
deallocate(xv2)

allocate(xv1(3,n))
allocate(xv2(3,n))

do i = 1, n
  xv1(1:3, i) = (/ real(i,krp),    2*real(i,krp), 1./real(i,krp) /)
  xv2(1:3, i) = (/ 3./real(i,krp), 3*real(i,krp), 1./real(i,krp) /)
enddo

!----------------------------------------------------------
call cpu_time(itime)
do i = 1, n
  r(i) = xv1(1,i)*xv2(1,i) + xv1(2,i)*xv2(2,i) + xv1(3,i)*xv2(3,i)
enddo
call cpu_time(time) ; dt = time-itime
print*,"v(1:3,i) .scal. inline    ",dt

!----------------------------------------------------------
call cpu_time(itime)
r(1:n) = sum(xv1*xv2, dim=1)
call cpu_time(time) ; dt = time-itime
print*,"v(1:3,i) .scal. vectorize ",dt

 

endprogram






program testvector

implicit none

integer j, n

print*,"..... test vector implementation"

n = 500
do j = 1, 14
  call vector(n)
  n = n * 2
enddo

endprogram

subroutine vector(n)

use TYPHMAKE
use GEO3D

implicit none

type(v3d), pointer :: v(:), w(:)
real(krp), pointer :: x(:)
real(krp), pointer :: tv(:,:), tw(:,:)
integer   :: i, n
real(krp) :: itime, time, t1, t2, t3, t4, t5, t6

allocate(v(n))
allocate(w(n))
allocate(x(n))

do i = 1, n
  x(i)    = cos(real(i,krp)/n)
  v(i)    = v3d(real(i,krp), x(i)*real(i,krp), 1./real(i,krp) )
enddo
!----------------------------------------------------------
call cpu_time(itime)
do i = 1, n
  w(i) = x(i)*v(i)
enddo
call cpu_time(time) ; t1 = time-itime

!----------------------------------------------------------
call cpu_time(itime)
do i = 1, n
  w(i)%x = x(i)*v(i)%x
  w(i)%y = x(i)*v(i)%y
  w(i)%z = x(i)*v(i)%z
enddo
call cpu_time(time) ; t2 = time-itime


!----------------------------------------------------------
call cpu_time(itime)
!w(1:n) = x(1:n)*v(1:n)
call cpu_time(time) ; t3 = time-itime

!----------------------------------------------------------
call cpu_time(itime)
call mult(w(1:n), x(1:n), v(1:n))
call cpu_time(time) ; t4 = time-itime

!----------------------------------------------------------
deallocate(v) ; allocate(tv(n,3))
deallocate(w) ; allocate(tw(n,3))

do i = 1, n
  tv(i,:) = (/ real(i,krp), x(i)*real(i,krp), 1./real(i,krp) /)
enddo

call cpu_time(itime)
do i = 1, n
  tw(i,:) = x(i)*tv(i,:)
enddo
call cpu_time(time) ; t5 = time-itime

call cpu_time(itime)
!tw(1:n,1) = x(1:n)*tv(1:n,1)
!tw(1:n,2) = x(1:n)*tv(1:n,2)
!tw(1:n,3) = x(1:n)*tv(1:n,3)
call cpu_time(time) ; t6 = time-itime

write(*,'(i10,6f9.6)') n, t1, t2, t3, t4, t5, t6

deallocate(tv, tw, x)

contains

subroutine mult(tv, x, v) 
!use TYPHMAKE
!use GEO3D
implicit none
real(krp), dimension(:), intent(in) :: x
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))       :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = x(i) * v(i)%x 
    tv(i)%y = x(i) * v(i)%y 
    tv(i)%z = x(i) * v(i)%z 
  enddo

end subroutine mult

endsubroutine vector



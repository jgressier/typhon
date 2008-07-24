program check_qr

use TYPHMAKE
use MATRIX

implicit none

integer, parameter :: n = 1024
real(krp) :: a(n, n), qr(n,n), b(n), sol(n), x(n), it, t
real(krp) :: newb(n), news(n)
real(krp) :: c(n),d(n)
logical :: sing
integer :: i,j,np

np = n

! fill vector sol and symmetric matrix a

do i = 1, n
  sol(i) = .1+cos(real(i))
  do j = 1, i
    a(i,j) = 10./(1+i-j)
    a(j,i) = a(i,j)
  enddo
enddo

! compute b = a.sol

b(1:n) = matmul(a, sol)
qr = a
x = b

! solve a.x = b

call cpu_time(it)
call qrdecomp(qr,n,np,c,d,sing)
call qrsolve(qr,n,np,c,d,x)
do i=1,8
  newb = b-matmul(a,x)
  print*, sum(abs(x-sol))/sum(abs(x)), sum(abs(newb))
  news = newb
  call qrsolve(qr,n,np,c,d,news)
  x = x+news
enddo
call cpu_time(t)

! print %error

print*, sum(abs(x-sol))/sum(abs(x)), t-it

end program check_qr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine qrupdate(r,qt,n,np,u,v)

use TYPHMAKE

implicit none

integer, intent(in) :: n,np
real(krp), intent(inout) :: r(np,np),u(np)
real(krp), intent(in) :: qt(np,np),v(np)

integer :: i,k

k = n
do while ( u(k) /= 0 .and. k > 1)
  k = k-1
enddo

do i=k-1,1,-1
  call rotate(r,qt,n,np,i,u(i),-u(i+1))
  if ( u(i) == 0.0_krp ) then
    u(i) = abs(u(i+1))
  else if ( abs(u(i)) > abs(u(i+1)) ) then
    u(i) = abs(u(i))*sqrt(1.0+(u(i+1)/u(i))**2)
  else
    u(i) = abs(u(i+1))*sqrt(1.0+(u(i)/u(i+1))**2)
  endif
enddo

r(1,1:n) = r(1,1:n) + u(1)*v(1:n)

do i=1,k-1
  call rotate(r,qt,n,np,i,r(i,i),-r(i+1,i))
enddo

end subroutine qrupdate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rotate(r,qt,n,np,i,a,b)

use TYPHMAKE

implicit none

integer, intent(in) :: n,np,i
real(krp), intent(in) :: a,b
real(krp), intent(inout) :: r(np,np),qt(np,np)

integer :: j
real(krp) :: c,fact,s,w,y

if ( a == 0 ) then
  c = 0.0_krp
  s = sign(1.0_krp,b)
else if ( abs(a) > abs(b) ) then
  fact = b/a
  c = sign(1.0/sqrt(1.0+fact*fact),a)
  s = fact*c
else
  fact = a/b
  s = sign(1.0/sqrt(1.0+fact*fact),b)
  c = fact*s
endif

do j=1,n
  y = r(i  ,j)
  w = r(i+1,j)
  r(i  ,j) = c*y-s*w
  r(i+1,j) = s*y+c*w
enddo

do j=1,n
  y = qt(i  ,j)
  w = qt(i+1,j)
  qt(i  ,j) = c*y-s*w
  qt(i+1,j) = s*y+c*w
enddo

end subroutine rotate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine qrsolve(a,n,np,c,d,b)

use TYPHMAKE

implicit none

integer, intent(in) :: n,np
real(krp), intent(in) :: a(np,np),c(n),d(n)
real(krp), intent(inout) :: b(n)

integer :: i,j
real(krp) :: ssum,tau

do j = 1,n-1
  ssum = 0.0_krp
  do i = j,n
    ssum = ssum+a(i,j)*b(i)
  enddo
  tau = ssum/c(j)
  do i = j,n
    b(i) = b(i)-tau*a(i,j)
  enddo
enddo

call rsolve(a,n,np,d,b)

end subroutine qrsolve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rsolve(a,n,np,d,b)

use TYPHMAKE

implicit none

integer, intent(in) :: n,np
real(krp), intent(in) :: a(np,np),d(n)
real(krp), intent(inout) :: b(n)

integer :: i,j
real(krp) :: ssum

b(n) = b(n)/d(n)
do i = n-1,1,-1
  ssum = 0.0_krp
  do j = i+1,n
    ssum = ssum+a(i,j)*b(j)
  enddo
  b(i) = (b(i)-ssum)/d(i)
enddo

end subroutine rsolve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine qrdecomp(a,n,np,c,d,sing)

use TYPHMAKE

implicit none

integer, intent(in) :: n,np
real(krp), intent(inout) :: a(np,np)
real(krp), intent(out) :: c(n),d(n)
logical, intent(out) :: sing

integer :: i,j,k
real(krp) :: scale,sigma,ssum,tau

sing = .false.

do k=1,n-1
  scale = 0.0_krp
  do i=k,n
    scale = max(scale,abs(a(i,k)))
  enddo
  if ( scale == 0.0_krp ) then
    sing = .true.
    c(k) = 0.0_krp
    d(k) = 0.0_krp
  else
    do i=k,n
      a(i,k) = a(i,k)/scale
    enddo
    ssum = 0.0_krp
    do i=k,n
      ssum = ssum+a(i,k)*a(i,k)
    enddo
    sigma = sign(sqrt(ssum),a(k,k))
    a(k,k) = a(k,k)+sigma
    c(k) =sigma*a(k,k)
    d(k) = -scale*sigma
    do j=k+1,n
      ssum = 0.0_krp
      do i=k,n
        ssum = ssum+a(i,k)*a(i,j)
      enddo
      tau = ssum/c(k)
      do i=k,n
        a(i,j) = a(i,j)-tau*a(i,k)
      enddo
    enddo
  endif
enddo

d(n) = a(n,n)

if ( d(n) == 0.0_krp ) sing = .true.

end subroutine qrdecomp

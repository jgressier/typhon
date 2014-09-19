program cholesky

use TYPHMAKE
use MATRIX

integer, parameter :: n = 1000
real(krp) :: a(n, n), b(n), x(n), it, t
integer i

do i = 1, n
  x(i) = .1+cos(real(i))
  do j = 1, i
    a(i,j) = 10./(1+i-j)
    a(j,i) = a(i,j)
  enddo
enddo

b(1:n) = matmul(a, x)

call cpu_time(it)
call cholesky_decomp(a, n)
call cholesky_solve(a, n, b, 1)
call cpu_time(t)

print*, sum(abs(x-b))/sum(abs(x)), t-it

endprogram cholesky

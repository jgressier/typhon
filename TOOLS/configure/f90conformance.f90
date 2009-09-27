module prec
  integer, parameter :: krp = 8
endmodule prec

program F90TEST

use prec

real(krp) :: x(100)
integer   :: i

do i = 1, 100
  x(i) = cos(real(i,krp))
enddo
x(1:100) = exp(x(1:100))

endprogram F90TEST

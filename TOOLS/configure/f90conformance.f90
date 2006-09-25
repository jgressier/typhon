module prec
  integer krp = 8
endmodule prec

program F90TEST

real(krp) :: x(100)
integer   :: i

do i = 1, 100
  x(i) = cos(i)
enddo
x(1:100) = exp(x(1:100))

endprogram F90TEST

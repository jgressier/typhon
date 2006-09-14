program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

integer, parameter     :: prec = 8
character(len=100)     :: str
real(prec)              :: x

x  = 4.2_prec
str="4.2"
call test_real(str, x)

x  = .03_prec
str=".03"
call test_real(str, x)

x  = -.03_prec
str="-.03"
call test_real(str, x)

x  = 1E2_prec
str="1E2"
call test_real(str, x)

x  = 1E+2_prec
str="1E+2"
call test_real(str, x)

x  = 1E-2_prec
str="1E-2"
call test_real(str, x)

x  = 2_prec+3_prec
str="2+3"
call test_real(str, x)

x  = 1.5_prec-.5_prec
str="1.5-.5"
call test_real(str, x)

x  = .2_prec*1E2_prec
str=".2*1E2"
call test_real(str, x)

x  = 1._prec/1E2_prec
str="1./1E2"
call test_real(str, x)

x  = 10.**3
str="10.^3"
call test_real(str, x)

x  = 10-2*3
str="10-2*3."
call test_real(str, x)

x  = (10-2)*3.
str="(10-2)*3"
call test_real(str, x)

x  = (1./2.)**2
str="sqr(inv(2.))"
call test_real(str, x)

x  = 2.**4*5
str="2^4*5"
call test_real(str, x)

x  = 10+2.**4*5.**3+5
str="10+2^4*5^3+5"
call test_real(str, x)

x  = 1./2.+2.**2
str="inv(2.)+sqr(2.)"
call test_real(str, x)

x  = sqrt(2._prec)
str="sqrt(2.)"
call test_real(str, x)

x  = exp(2._prec**2*log(2._prec))
str="exp(2.^2*ln(2))"
call test_real(str, x)

x  = tan(1._prec)
str="tan(1.)"
call test_real(str, x)

x  = cos(1._prec)
str="cos(1.)"
call test_real(str, x)

x  = sin(1._prec)
str="sin(1.)"
call test_real(str, x)

x  = tanh(1._prec)
str="tanh(1.)"
call test_real(str, x)

x  = cosh(1._prec)
str="cosh(1.)"
call test_real(str, x)

x  = sinh(1._prec)
str="sinh(1.)"
call test_real(str, x)

x  = atan(.5_prec)
str="atan(.5)"
call test_real(str, x)

x  = acos(.5_prec)
str="acos(.5)"
call test_real(str, x)

x  = asin(.5_prec)
str="asin(.5)"
call test_real(str, x)

x  = 10.-3.-2.
str="10.-3.-2."
call test_real(str, x)

x  = 10._prec/3._prec/2._prec
str="10./3./2."
call test_real(str, x)

x  = ((3+2)*5+4/2)*3+(3+2./4.)
str="( (3+2)*5 + 4/2 )*3 + (3+2./4.)"
call test_real(str, x)

x  = (((3**2+2)*5+4./2.)*3-(3-2./4.)*4)/(2-4)**2
str="(((3^2+2)*5+4./2.)*3-(3-2./4.)*4)/(2-4)^2"
call test_real(str, x)

contains

subroutine test_real(str, x)
implicit none
character(len=100)     :: str
type(st_fct_node)      :: func
type(st_fct_container) :: cont
real(prec)             :: x
character(len=20)  :: form="(a30,2g16.8,2x,a8)"
character(len=20)  :: err
integer            :: ierr

call string_to_funct(str, func, ierr)
call fct_node_eval(blank_env, func, cont)
if ((abs(cont%r-x)/x) > 10*sqrt(epsilon(x))) then
  err = "FAUX"
  print form, str, cont%r, x, err
  call fct_node_to_str(func, str)
  print*,"->",trim(str)
elseif ((abs(cont%r-x)/x) > 10*epsilon(x)) then
  err = "Mauvais"
  print form, str, cont%r, x, err
else
  err = " Ok "
  print form, str, cont%r, x, err
endif
call delete(cont)
call delete(func)

endsubroutine

endprogram

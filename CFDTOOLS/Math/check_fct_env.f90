program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

type(st_fct_env)       :: env
integer, parameter     :: prec = 8
character(len=128)     :: str
real(prec)             :: x, gamma, mach, r

call new_fct_env(env)

x  = 10._prec
call fct_env_set_real(env, "x", x)
call print_fct_env(6, env)

r  = exp(x)/x 
str="exp(x)/x"
call test_real(str, r)

gamma = 1.4_prec
call fct_env_set_real(env, "Gamma", gamma)
mach = 2._prec
call fct_env_set_real(env, "MACH", mach)

call print_fct_env(6, env)

r  = (1.+(gamma-1)/2*mach**2)**(gamma/(gamma-1))
str="(1.+(gamma-1)/2*sqr(mach))^(gamma/(gamma-1))"
call test_real(str, r)

contains

subroutine test_real(str, x)
implicit none
character(len=128)     :: str
type(st_fct_node)      :: func
type(st_fct_container) :: cont
real(prec)             :: x
integer                :: strlen
character(len=32)  :: form !="(a33,a1,2g16.8,2x,a8)"
character(len=16)  :: err
integer            :: ierr

strlen = 32
if (len(trim(str)) > strlen) strlen = len(trim(str))
strlen = strlen + 1
write(form,"(a2,i3.3,a17)") "(a",strlen,",a1,2g16.8,2x,a8)"

call string_to_funct(str, func, ierr)
call fct_node_eval(env, func, cont)
if ((abs(cont%r-x)/x) > 10*sqrt(epsilon(x))) then
  err = "FAUX"
elseif ((abs(cont%r-x)/x) > 10*epsilon(x)) then
  err = "Mauvais"
else
  err = " Ok "
endif
print form, str, ":", cont%r, x, err
call delete(cont)
call delete(func)

endsubroutine

endprogram

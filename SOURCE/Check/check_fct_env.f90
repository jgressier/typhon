program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

type(st_fct_env)       :: env
integer, parameter     :: prec = 8
character(len=100)     :: str
real(prec)             :: x, gamma, mach, r

call new_fct_env(env)

x  = 2._prec
call fct_env_set_real(env, "x", x)
print*,'ENV'
call print_fct_env(6, env)

x  = 10._prec
call fct_env_set_real(env, "x", x)
print*,'ENV'
call print_fct_env(6, env)

r  = exp(x)/x 
str="exp(x)/x"
call test_real(str, r)

gamma = 1.4
call fct_env_set_real(env, "Gamma", gamma)
mach = 2._prec
call fct_env_set_real(env, "MACH", mach)

print*,'ENV'
call print_fct_env(6, env)

r  = (1.+(gamma-1)/2*mach**2)**(gamma/(gamma-1))
str="(1.+(gamma-1)/2*sqr(mach))^(gamma/(gamma-1))"
call test_real(str, r)



contains

subroutine test_real(string, res)
implicit none
character(len=100)     :: string
type(st_fct_node)      :: func
type(st_fct_container) :: cont
real(prec)             :: res
character(len=20)  :: form="(a30,2g16.8,2x,a8)"
character(len=20)  :: err

call string_to_node(string, func)
call fct_node_eval(env, func, cont)
if ((abs(cont%r-res)/res) > 10*sqrt(epsilon(res))) then
  err = "FAUX"
elseif ((abs(cont%r-res)/res) > 10*epsilon(res)) then
  err = "Mauvais"
else
  err = " Ok "
endif
print form, string, cont%r, res, err
call delete(cont)
call delete(func)

endsubroutine

endprogram

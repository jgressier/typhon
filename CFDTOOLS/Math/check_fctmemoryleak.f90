program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

type(st_fct_env)       :: env
integer, parameter     :: prec = 8
character(len=128)     :: str
real(prec)             :: x, res
type(st_fct_node)      :: func
integer                :: ierr, i

str="exp(x)/x"
call string_to_funct(str, func, ierr)

i = 0 

do while (.true.)

  i = i + 1
  print*, i
  call new_fct_env(env)
  x  = 10._prec
  call fct_env_set_real(env, "x", x)
  call fct_eval_real(env, func, res)
  call delete_fct_env(env)

enddo

endprogram

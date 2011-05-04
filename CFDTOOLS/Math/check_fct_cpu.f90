program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

type(st_fct_env)       :: env
type(st_fct_node)      :: func
type(st_fct_container) :: cont
integer, parameter     :: prec = 8
integer                :: i, n
real                   :: t1, t0
real(prec), pointer    :: x(:), t(:)
integer                :: ierr
character(len=100)     :: str

call new_fct_env(env)

n = 5000000
allocate(x(n), t(n))

!----------------------------------------------
str="ln(x)/x"
print*
print*,'* '//trim(str)

call cpu_time(t0)
do i = 1, n
  x(i) = real(i)
  t(i) = log(x(i))/x(i)
enddo
call cpu_time(t1)
print*,"direct computation loop - cpu:",t1-t0

call string_to_funct(str, func, ierr)
call cpu_time(t0)
do i = 1, n
  x(i) = real(i)
  call fct_env_set_real(env, "x", x(i))
  call fct_node_eval(env, func, cont)
  t(i) = cont%r
enddo
call cpu_time(t1)
print*,"fct eval           loop - cpu:",t1-t0

call delete(cont)
call delete(func)

!----------------------------------------------
str="exp(x/1000.)/sqrt(1.+x^2)+sin(x)*cos(x)"
print*
print*,'* '//trim(str)

call cpu_time(t0)
do i = 1, n
  x(i) = real(i)
  t(i) = exp(x(i)/1000.)/sqrt(1.+x(i)**2)+sin(x(i))*cos(x(i))
enddo
call cpu_time(t1)
print*,"direct computation loop - cpu:",t1-t0

call string_to_funct(str, func, ierr)
call cpu_time(t0)
do i = 1, n
  x(i) = real(i)
  call fct_env_set_real(env, "x", x(i))
  call fct_node_eval(env, func, cont)
  t(i) = cont%r
enddo
call cpu_time(t1)
print*,"fct eval           loop - cpu:",t1-t0

call delete(cont)
call delete(func)

!----------------------------------------------
str="10."
print*
print*,'* '//trim(str)

call string_to_funct(str, func, ierr)
call cpu_time(t0)
do i = 1, n
  x(i) = real(i)
  call fct_env_set_real(env, "x", x(i))
  call fct_node_eval(env, func, cont)
  t(i) = cont%r
enddo
call cpu_time(t1)
print*,"fct cst+set loop - cpu:",t1-t0

call cpu_time(t0)
do i = 1, n
  call fct_node_eval(env, func, cont)
  t(i) = cont%r
enddo
call cpu_time(t1)
print*,"fct cst     loop - cpu:",t1-t0

call delete(cont)
call delete(func)


endprogram

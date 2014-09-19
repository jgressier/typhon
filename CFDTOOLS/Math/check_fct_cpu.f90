program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

type(st_fct_env)       :: env
type(st_fct_node)      :: func
!type(st_fct_container) :: cont
integer, parameter     :: prec = 8
integer                :: i, n, s, d
real                   :: t1, t0
real(prec), pointer    :: x(:), t(:)
integer                :: ierr
character(len=100)     :: str


n = 2**22
!n = 5000
allocate(x(n), t(n))

do i = 1, n
  x(i) = real(i)/100.
enddo

!----------------------------------------------
str="ln(x)/x"
print*
print*,'* '//trim(str)

call cpu_time(t0)
do i = 1, n
  t(i) = log(x(i))/x(i)
enddo
call cpu_time(t1)
print*,"direct computation loop - cpu:",t1-t0

!call cpu_time(t0)
!t = log(x)/x
!call cpu_time(t1)
!print*,"direct computation vect - cpu:",t1-t0

call string_to_funct(str, func, ierr)
call cpu_time(t0)
call new_fct_env(env)
do i = 1, n
  call fct_env_set_real(env, "x", x(i))
  call fct_eval_real(env, func, t(i))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct eval           loop - cpu:",t1-t0

do s = 1, 20
d = 2**s
call cpu_time(t0)
call new_fct_env(env)
do i = 1, n/d
  !print*,n,d,n/d,(i-1)*d+1,i*d
  call fct_env_set_realarray(env, "x", x((i-1)*d+1:i*d))
  call fct_eval_realarray(env, func, t((i-1)*d+1:i*d))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct eval array ",n/d,'x',d," loop - cpu:",t1-t0
enddo

!call delete(cont)
call delete(func)

!----------------------------------------------
str="exp(x/1000.)/sqrt(1.+x^2)+sin(x)*cos(x)"
print*
print*,'* '//trim(str)

call cpu_time(t0)
do i = 1, n
  t(i) = exp(x(i)/1000.)/sqrt(1.+x(i)**2)+sin(x(i))*cos(x(i))
enddo
call cpu_time(t1)
print*,"direct computation loop - cpu:",t1-t0

!call cpu_time(t0)
!t = exp(x/1000.)/sqrt(1.+x**2)+sin(x)*cos(x)
!call cpu_time(t1)
!print*,"direct computation vect - cpu:",t1-t0

call string_to_funct(str, func, ierr)
call cpu_time(t0)
call new_fct_env(env)
do i = 1, n
  call fct_env_set_real(env, "x", x(i))
  call fct_eval_real(env, func, t(i))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct eval           loop - cpu:",t1-t0

do s = 1, 20
d = 2**s
call cpu_time(t0)
call new_fct_env(env)
do i = 1, n/d
  !print*,n,d,n/d,(i-1)*d+1,i*d
  call fct_env_set_realarray(env, "x", x((i-1)*d+1:i*d))
  call fct_eval_realarray(env, func, t((i-1)*d+1:i*d))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct eval array ",n/d,'x',d," loop - cpu:",t1-t0
enddo

!call delete(cont)
call delete(func)

!----------------------------------------------
str="10."
print*
print*,'* '//trim(str)

call string_to_funct(str, func, ierr)
call cpu_time(t0)
call new_fct_env(env)
do i = 1, n
  call fct_env_set_real(env, "x", x(i))
  call fct_eval_real(env, func, t(i))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct cst+set loop - cpu:",t1-t0

call cpu_time(t0)
call new_fct_env(env)
do i = 1, n
  call fct_eval_real(env, func, t(i))
enddo
call delete_fct_env(env)
call cpu_time(t1)
print*,"fct cst     loop - cpu:",t1-t0

!call delete(cont)
call delete(func)


endprogram test_fct

program test_fct

use FCT_PARSER
use FCT_EVAL
use FCT_ENV

implicit none

type(st_fct_env)       :: env
integer, parameter     :: prec = 8
character(len=128)     :: str
real(prec)             :: x, res
type(st_fct_node)      :: func
integer                :: ierr, i, ni, imod, j, n10, n
character(len=64)      :: nzer
character(len=64)      :: spc=""
character(len=128)     :: bar=""

str="exp(x)/x"
call string_to_funct(str, func, ierr)

ni = 50

#ifdef __INTEL_COMPILER
  open (unit=6, carriagecontrol='fortran')
#endif
do i = 1, ni
  bar(i:i) = "="
enddo

n10 = 5
n = 10**n10
do i = 1, n10
  nzer(i:i) = "0"
enddo

i = 0 

write(6,fmt="(2a1,a12,a,2x,a1,a,a1)") ' ',char(13),' ',spc(1:n10),'|',spc(1:ni),'|'
do while (.true.)
  i = i + 1
  imod = mod(i-1,ni)+1
  if ( imod == 1 ) then
    write(6,'(a)') ''
    write(6,fmt="(2a1,i12,a,2x,a1,a,a1)") '+',char(13),i,nzer(1:n10),'|',spc(1:ni),'|'
  endif
  do j = 1, n
  call new_fct_env(env)
  x  = 10._prec
  call fct_env_set_real(env, "x", x)
  call fct_eval_real(env, func, res)
  call delete_fct_env(env)
  enddo
  write(6,fmt="(2a1,i12,a,2x,a1,a)") '+',char(13),i,nzer(1:n10),'|',bar(1:imod)
enddo

endprogram

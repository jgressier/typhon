!------------------------------------------------------------------------------!
! MODULE : FCT_MATH                         Authors : J. Gressier
!                                           Date    : July 2006
! Evaluation of operations between FCT_CONTAINERs
! !!! most operations CREATE containers (which should be deleted at the end) !!!
!------------------------------------------------------------------------------!

module FCT_MATH

use FCT_DEF
use FCT_CONTAINER

implicit none

! -- Constants -------------------------------------------

     
! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! fct_cont_add
!------------------------------------------------------------------------------!
subroutine fct_cont_add(res, left, right)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(left%type)

case(cont_real)            ! -- left OP is REAL -------------------------------
  select case(right%type)
  case(cont_real)                                  ! both OP are real
    call new_fct_container(res, cont_real, "")
    res%r = left%r + right%r
  case(cont_vect)                                  ! left is real ; right is real array
    call new_fct_container(res, cont_vect, "", right%size)
    res%r_t = left%r + right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in ADD operator")
  endselect

case(cont_vect)            ! -- left OP is REAL ARRAY -------------------------------
  select case(right%type)
  case(cont_real)                                  ! left is real array ; right OP is real
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t + right%r
  case(cont_vect)                                  ! both are real arrays
    if (left%size /= right%size) call set_fct_error(-1, "operands with different sizes in ADD operator")
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t + right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands type in ADD operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands type in ADD operator")
endselect

endsubroutine fct_cont_add

!------------------------------------------------------------------------------!
! fct_cont_sub
!------------------------------------------------------------------------------!
subroutine fct_cont_sub(res, left, right)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(left%type)

case(cont_real)            ! -- left OP is REAL -------------------------------
  select case(right%type)
  case(cont_real)                                  ! both OP are real
    call new_fct_container(res, cont_real, "")
    res%r = left%r - right%r
  case(cont_vect)                                  ! left is real ; right is real array
    call new_fct_container(res, cont_vect, "", right%size)
    res%r_t = left%r - right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in SUB operator")
  endselect

case(cont_vect)            ! -- left OP is REAL ARRAY -------------------------------
  select case(right%type)
  case(cont_real)                                  ! left is real array ; right OP is real
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t - right%r
  case(cont_vect)                                  ! both are real arrays
    if (left%size /= right%size) call set_fct_error(-1, "operands with different sizes in SUB operator")
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t - right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands type in SUB operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in SUB operator")
endselect

endsubroutine fct_cont_sub

!------------------------------------------------------------------------------!
! fct_cont_mul
!------------------------------------------------------------------------------!
subroutine fct_cont_mul(res, left, right)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(left%type)

case(cont_real)            ! -- left OP is REAL -------------------------------
  select case(right%type)
  case(cont_real)                                  ! both OP are real
    call new_fct_container(res, cont_real, "")
    res%r = left%r * right%r
  case(cont_vect)                                  ! left is real ; right is real array
    call new_fct_container(res, cont_vect, "", right%size)
    res%r_t = left%r * right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in MULT operator")
  endselect

case(cont_vect)            ! -- left OP is REAL ARRAY -------------------------------
  select case(right%type)
  case(cont_real)                                  ! left is real array ; right OP is real
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t * right%r
  case(cont_vect)                                  ! both are real arrays
    if (left%size /= right%size) call set_fct_error(-1, "operands with different sizes in MULT operator")
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t * right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands type in MULT operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in MUL operator")
endselect

endsubroutine fct_cont_mul

!------------------------------------------------------------------------------!
! fct_cont_div
!------------------------------------------------------------------------------!
subroutine fct_cont_div(res, left, right)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(left%type)

case(cont_real)            ! -- left OP is REAL -------------------------------
  select case(right%type)
  case(cont_real)                                  ! both OP are real
    call new_fct_container(res, cont_real, "")
    res%r = left%r / right%r
  case(cont_vect)                                  ! left is real ; right is real array
    call new_fct_container(res, cont_vect, "", right%size)
    res%r_t = left%r / right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in DIV operator")
  endselect

case(cont_vect)            ! -- left OP is REAL ARRAY -------------------------------
  select case(right%type)
  case(cont_real)                                  ! left is real array ; right OP is real
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t / right%r
  case(cont_vect)                                  ! both are real arrays
    if (left%size /= right%size) call set_fct_error(-1, "operands with different sizes in DIV operator")
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t / right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in DIV operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in DIV operator")
endselect

endsubroutine fct_cont_div

!------------------------------------------------------------------------------!
! fct_cont_pow
!------------------------------------------------------------------------------!
subroutine fct_cont_pow(res, left, right)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(left%type)

case(cont_real)            ! -- left OP is REAL -------------------------------
  select case(right%type)
  case(cont_real)                                  ! both OP are real
    call new_fct_container(res, cont_real, "")
    res%r = left%r ** right%r
  case(cont_vect)                                  ! left is real ; right is real array
    call new_fct_container(res, cont_vect, "", right%size)
    res%r_t = left%r ** right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in POW operator")
  endselect

case(cont_vect)            ! -- left OP is REAL ARRAY -------------------------------
  select case(right%type)
  case(cont_real)                                  ! left is real array ; right OP is real
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t ** right%r
  case(cont_vect)                                  ! both are real arrays
    if (left%size /= right%size) call set_fct_error(-1, "operands with different sizes in POW operator")
    call new_fct_container(res, cont_vect, "", left%size)
    res%r_t = left%r_t ** right%r_t
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in POW operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in POW operator")
endselect

endsubroutine fct_cont_pow

!------------------------------------------------------------------------------!
! fct_cont_opp
!------------------------------------------------------------------------------!
subroutine fct_cont_opp(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = -op%r

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = -op%r_t

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in OPPOSITE operator")
endselect

endsubroutine fct_cont_opp

!------------------------------------------------------------------------------!
! fct_cont_inv
!------------------------------------------------------------------------------!
subroutine fct_cont_inv(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = 1._rprc / op%r

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = 1._krp / op%r_t

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in INV operator")
endselect

endsubroutine fct_cont_inv

!------------------------------------------------------------------------------!
! fct_cont_sqr
!------------------------------------------------------------------------------!
subroutine fct_cont_sqr(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = op%r**2

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = op%r_t**2

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in SQR operator")
endselect

endsubroutine fct_cont_sqr

!------------------------------------------------------------------------------!
! fct_cont_sqrt
!------------------------------------------------------------------------------!
subroutine fct_cont_sqrt(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = sqrt(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = sqrt(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in SQRT operator")
endselect

endsubroutine fct_cont_sqrt

!------------------------------------------------------------------------------!
! fct_cont_exp
!------------------------------------------------------------------------------!
subroutine fct_cont_exp(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = exp(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = exp(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in EXP operator")
endselect

endsubroutine fct_cont_exp

!------------------------------------------------------------------------------!
! fct_cont_ln
!------------------------------------------------------------------------------!
subroutine fct_cont_ln(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = log(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = log(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in LN operator")
endselect

endsubroutine fct_cont_ln

!------------------------------------------------------------------------------!
! fct_cont_log
!------------------------------------------------------------------------------!
subroutine fct_cont_log(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = log10(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = log10(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in LOG operator")
endselect

endsubroutine fct_cont_log

!------------------------------------------------------------------------------!
! fct_cont_sin
!------------------------------------------------------------------------------!
subroutine fct_cont_sin(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = sin(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = sin(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in SIN operator")
endselect

endsubroutine fct_cont_sin

!------------------------------------------------------------------------------!
! fct_cont_cos
!------------------------------------------------------------------------------!
subroutine fct_cont_cos(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = cos(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = cos(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in COS operator")
endselect

endsubroutine fct_cont_cos

!------------------------------------------------------------------------------!
! fct_cont_tan
!------------------------------------------------------------------------------!
subroutine fct_cont_tan(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = tan(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = tan(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in TAN operator")
endselect

endsubroutine fct_cont_tan

!------------------------------------------------------------------------------!
! fct_cont_sinh
!------------------------------------------------------------------------------!
subroutine fct_cont_sinh(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = sinh(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = sinh(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in SINH operator")
endselect

endsubroutine fct_cont_sinh

!------------------------------------------------------------------------------!
! fct_cont_cosh
!------------------------------------------------------------------------------!
subroutine fct_cont_cosh(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = cosh(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = cosh(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in COSH operator")
endselect

endsubroutine fct_cont_cosh

!------------------------------------------------------------------------------!
! fct_cont_tanh
!------------------------------------------------------------------------------!
subroutine fct_cont_tanh(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = tanh(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = tanh(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in TANH operator")
endselect

endsubroutine fct_cont_tanh

!------------------------------------------------------------------------------!
! fct_cont_asin
!------------------------------------------------------------------------------!
subroutine fct_cont_asin(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = asin(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = asin(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ASIN operator")
endselect

endsubroutine fct_cont_asin

!------------------------------------------------------------------------------!
! fct_cont_acos
!------------------------------------------------------------------------------!
subroutine fct_cont_acos(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = acos(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = acos(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ACOS operator")
endselect

endsubroutine fct_cont_acos

!------------------------------------------------------------------------------!
! fct_cont_atan
!------------------------------------------------------------------------------!
subroutine fct_cont_atan(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = atan(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = atan(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ATAN operator")
endselect

endsubroutine fct_cont_atan

!------------------------------------------------------------------------------!
! fct_cont_abs
!------------------------------------------------------------------------------!
subroutine fct_cont_abs(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  res%r = abs(op%r)

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  res%r_t = abs(op%r_t)

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ABSOLUTE operator")
endselect

endsubroutine fct_cont_abs

!------------------------------------------------------------------------------!
! fct_cont_step
!------------------------------------------------------------------------------!
subroutine fct_cont_step(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  if (op%r >= 0._rprc) then
    res%r = 1._rprc
  else
    res%r = 0._rprc
  endif

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  where (op%r_t >= 0._rprc)
    res%r_t = 1._rprc
  elsewhere
    res%r_t = 0._rprc
  endwhere
  
case default
  call set_fct_error(-1, "incorrect or non-implemented operands in STEP operator")
endselect

endsubroutine fct_cont_step

!------------------------------------------------------------------------------!
! fct_cont_ramp
!------------------------------------------------------------------------------!
subroutine fct_cont_ramp(res, op)
implicit none
! -- parameters --
type(st_fct_container), intent(in)  :: op            ! operand
type(st_fct_container), intent(out) :: res           ! container result
! -- internal variables --

! -- body --

select case(op%type)

case(cont_real)            ! -- op OP is REAL -------------------------------
  call new_fct_container(res, cont_real, "")
  if (op%r >= 0._rprc) then
    res%r = op%r
  else
    res%r = 0._rprc
  endif

case(cont_vect)            ! -- op OP is REAL ARRAY -------------------------------
  call new_fct_container(res, cont_vect, "", op%size)
  where (op%r_t >= 0._rprc)
    res%r_t = op%r_t
  elsewhere
    res%r_t = 0._rprc
  endwhere
  
case default
  call set_fct_error(-1, "incorrect or non-implemented operands in RAMP operator")
endselect

endsubroutine fct_cont_ramp


!------------------------------------------------------------------------------!
endmodule FCT_MATH
!------------------------------------------------------------------------------!
! Changes history
!
! July 2006 : module creation
! Aug  2006 : add step function
!------------------------------------------------------------------------------!

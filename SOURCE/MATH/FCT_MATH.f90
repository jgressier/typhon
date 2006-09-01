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
  case(cont_real)                                  ! right OP is real
    call new_fct_container(res, cont_real, "")
    res%r = left%r + right%r
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in ADD operator")
  endselect

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ADD operator")
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
  case(cont_real)                                  ! right OP is real
    call new_fct_container(res, cont_real, "")
    res%r = left%r - right%r
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in SUB operator")
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
  case(cont_real)                                  ! right OP is real
    call new_fct_container(res, cont_real, "")
    res%r = left%r * right%r
  case default
    call set_fct_error(-1, "incorrect or non-implemented operands in MUL operator")
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
  case(cont_real)                                  ! right OP is real
    call new_fct_container(res, cont_real, "")
    res%r = left%r / right%r
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
  case(cont_real)                                  ! right OP is real
    call new_fct_container(res, cont_real, "")
    res%r = left%r ** right%r
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
  res%r = 1._rprc/op%r

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

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ATAN operator")
endselect

endsubroutine fct_cont_atan

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

case default
  call set_fct_error(-1, "incorrect or non-implemented operands in ATAN operator")
endselect

endsubroutine fct_cont_step


!------------------------------------------------------------------------------!
endmodule FCT_MATH
!------------------------------------------------------------------------------!
! Changes history
!
! July 2006 : module creation
! Aug  2006 : add step function
!------------------------------------------------------------------------------!

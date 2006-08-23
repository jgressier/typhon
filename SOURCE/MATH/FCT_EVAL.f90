!------------------------------------------------------------------------------!
! MODULE : FCT_EVAL                         Authors : J. Gressier
!                                           Date    : March 2006
! Evaluation of a function 
!
!------------------------------------------------------------------------------!

module FCT_EVAL

use FCT_DEF
use FCT_ENV
use FCT_MATH

implicit none

! -- Constants -------------------------------------------

type(st_fct_env) :: blank_env
     
! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_EVAL : 
!------------------------------------------------------------------------------!
!type st_fct_eval
!  integer                 :: type_node
!endtype st_fct_eval


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! fct_node_eval
!------------------------------------------------------------------------------!
recursive subroutine fct_node_eval(env, fct, res)
implicit none

! -- parameters --
type(st_fct_env),       intent(in)  :: env     ! environment
type(st_fct_node),      intent(in)  :: fct     ! function to evaluate (base node)
type(st_fct_container), intent(out) :: res     ! container result

! -- internal variables --
type(st_fct_container) :: left, right          ! evaluation of possible left & right operands

! -- body --

select case(fct%type_node)

case(node_cst)
  call copy_fct_container(fct%container, res)

case(node_var)
  ! should be evaluated in ENV
  call set_fct_error(-1, "computation with environment variables not implemented in FCT_EVAL")

case(node_opunit)
  call fct_node_eval(env, fct%left, left)
  call fct_node_eval_opunit(env, fct%type_oper, left, res)
  call delete(left)

case(node_opbin)
  call fct_node_eval(env, fct%left,  left)
  call fct_node_eval(env, fct%right, right)
  call fct_node_eval_opbin(env, fct%type_oper, left, right, res)
  call delete(left)
  call delete(right)

case default
  call set_fct_error(-1, "unknown NODE type in FCT_EVAL")
endselect

endsubroutine fct_node_eval


!------------------------------------------------------------------------------!
! fct_node_eval_opbin
!------------------------------------------------------------------------------!
subroutine fct_node_eval_opbin(env, type_oper, left, right, res)
implicit none

! -- parameters --
type(st_fct_env),       intent(in)  :: env
integer(ipar),          intent(in)  :: type_oper     ! type of binary operator
type(st_fct_container), intent(in)  :: left, right   ! both operands
type(st_fct_container), intent(out) :: res           ! container result

! -- internal variables --

! -- body --

select case(type_oper)
case(op_add)
  call fct_cont_add(res, left, right)
case(op_sub)
  call fct_cont_sub(res, left, right)
case(op_mul)
  call fct_cont_mul(res, left, right)
case(op_div)
  call fct_cont_div(res, left, right)
case(op_pow)
  call fct_cont_pow(res, left, right)
case default
  call set_fct_error(-1, "unknown or non-implemented BINARY OPERATOR in FCT_EVAL")
endselect

endsubroutine fct_node_eval_opbin


!------------------------------------------------------------------------------!
! fct_node_eval_opunit
!------------------------------------------------------------------------------!
subroutine fct_node_eval_opunit(env, type_oper, operand, res)
implicit none

! -- parameters --
type(st_fct_env),       intent(in)  :: env
integer(ipar),          intent(in)  :: type_oper     ! type of unary operator
type(st_fct_container), intent(in)  :: operand       ! operand 
type(st_fct_container), intent(out) :: res           ! container result

! -- internal variables --

! -- body --

select case(type_oper)
case(fct_opp)
  call fct_cont_opp(res, operand)
case(fct_inv)
  call fct_cont_inv(res, operand)
case(fct_sqr)
  call fct_cont_sqr(res, operand)
case(fct_sqrt)
  call fct_cont_sqrt(res, operand)
case(fct_exp)
  call fct_cont_exp(res, operand)
case(fct_ln)
  call fct_cont_ln(res, operand)
case(fct_log)
  call fct_cont_log(res, operand)
case(fct_sin)
  call fct_cont_sin(res, operand)
case(fct_cos)
  call fct_cont_cos(res, operand)
case(fct_tan)
  call fct_cont_tan(res, operand)
case(fct_sinh)
  call fct_cont_sinh(res, operand)
case(fct_cosh)
  call fct_cont_cosh(res, operand)
case(fct_tanh)
  call fct_cont_tanh(res, operand)
case(fct_asin)
  call fct_cont_asin(res, operand)
case(fct_acos)
  call fct_cont_acos(res, operand)
case(fct_atan)
  call fct_cont_atan(res, operand)
!case(fct_asinh)
!  call fct_cont_inv(res, operand)
!case(fct_acosh)
!  call fct_cont_inv(res, operand)
!case(fct_atanh)
!  call fct_cont_inv(res, operand)
!case(fct_abs)
!  call fct_cont_inv(res, operand)
!case(fct_sign)
!  call fct_cont_inv(res, operand)
!case(fct_step)
!  call fct_cont_inv(res, operand)
case default
  call set_fct_error(-1, "unknown or non-implemented UNARY OPERATOR in FCT_MATH")
endselect

endsubroutine fct_node_eval_opunit


!------------------------------------------------------------------------------!
endmodule FCT_EVAL
!------------------------------------------------------------------------------!
! Changes history
!
! May  2006 : module creation
! July 2006 : evaluation of fct_node tree with real operands only
!------------------------------------------------------------------------------!

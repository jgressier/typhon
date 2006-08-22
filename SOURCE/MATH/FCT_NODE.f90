!------------------------------------------------------------------------------!
! MODULE : FCT_NODE                       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Definition of NODE of the tree
!     contains type, links and CONTAINER
!
!------------------------------------------------------------------------------!

module FCT_NODE

use FCT_DEF         ! accuracy definition and error handlers
use FCT_CONTAINER   ! definition of container

implicit none

! -- Constants -------------------------------------------

! -- node type

integer(ipar), parameter :: node_cst    = 0
integer(ipar), parameter :: node_var    = 5
integer(ipar), parameter :: node_opunit = 1
integer(ipar), parameter :: node_opbin  = 2

! -- unitary operators

integer(ipar), parameter :: fct_inv   = 01
integer(ipar), parameter :: fct_sqr   = 02
integer(ipar), parameter :: fct_sqrt  = 03
integer(ipar), parameter :: fct_exp   = 04
integer(ipar), parameter :: fct_ln    = 05
integer(ipar), parameter :: fct_log   = 06
integer(ipar), parameter :: fct_sin   = 07
integer(ipar), parameter :: fct_cos   = 08
integer(ipar), parameter :: fct_tan   = 09
integer(ipar), parameter :: fct_sinh  = 10
integer(ipar), parameter :: fct_cosh  = 11
integer(ipar), parameter :: fct_tanh  = 12
integer(ipar), parameter :: fct_asin  = 13
integer(ipar), parameter :: fct_acos  = 14
integer(ipar), parameter :: fct_atan  = 15
integer(ipar), parameter :: fct_asinh = 16
integer(ipar), parameter :: fct_acosh = 17
integer(ipar), parameter :: fct_atanh = 18
integer(ipar), parameter :: fct_abs   = 19
integer(ipar), parameter :: fct_sign  = 20
integer(ipar), parameter :: fct_step  = 21
integer(ipar), parameter :: min_fct   = 01
integer(ipar), parameter :: max_fct   = fct_step

! -- binary operators (reverse priority order) --

integer(ipar), parameter :: op_and = 01
integer(ipar), parameter :: op_or  = 02
integer(ipar), parameter :: op_neq = 03
integer(ipar), parameter :: op_eq  = 04
integer(ipar), parameter :: op_lt  = 05
integer(ipar), parameter :: op_gt  = 06
integer(ipar), parameter :: op_leq = 07
integer(ipar), parameter :: op_geq = 08
integer(ipar), parameter :: op_add = 09
integer(ipar), parameter :: op_sub = 10
integer(ipar), parameter :: op_mul = 11
integer(ipar), parameter :: op_div = 12
integer(ipar), parameter :: op_pow = 13
integer(ipar), parameter :: min_op = op_and
integer(ipar), parameter :: max_op = op_pow

! -- associated strings --

integer(ipar), parameter :: fct_len = 5
character(len=fct_len), dimension(fct_inv:max_fct), parameter :: op1name = & 
  (/ "inv  ", "sqr  ", "sqrt ", "exp  ", "ln   ", "log  ", &
     "sin  ", "cos  ", "tan  ", "sinh ", "cosh ", "tanh ", &
     "asin ", "acos ", "atan ", "asinh", "acosh", "atanh", &
     "abs  ", "sign ", "step "/)
     
integer(ipar), parameter :: op_len = 2
character(len=op_len), dimension(min_op:max_op), parameter :: op2name = &
  (/ "&&", "||", "!=", "==", "< ", "> ", "<=", ">=", &
     "+ ", "- ", "* ", "/ ", "^ " /)
     
!integer(ipar), dimension(op_add:max_op), parameter :: op2priority = &
!  (/   50,   51,   60,   61,  100,   40,   40, &
!       30,   30,   30,   30,   30,   30 /) 

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_NODE : NODE element of equation TREE
!------------------------------------------------------------------------------!
type st_fct_node
  integer(ipar)              :: type_node            ! type of node : operator/container
  integer(ipar)              :: type_oper            ! if node : type of operator
  integer(ipar)              :: size
  type(st_fct_container)          :: container
  type(st_fct_node),      pointer :: left, right
endtype st_fct_node


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_fct_node
endinterface

interface delete
  module procedure delete_fct_node
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION -----


!----------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_node : allocate FCT_NODE structure
!------------------------------------------------------------------------------!
subroutine new_fct_node(node, type, oper)
implicit none
! - parameters
type(st_fct_node)       :: node
integer(ipar)           :: type       ! type of container
integer(ipar), optional :: oper

  node%type_node = type

  nullify(node%left)          ! default initialization
  nullify(node%right)         ! default initialization

  select case(type)
  case(node_cst)
    call new_fct_container(node%container, cont_real)
  case(node_var)
    call new_fct_container(node%container, cont_var)
  case(node_opunit)
    node%type_oper = oper
    allocate(node%left)
  case(node_opbin)
    node%type_oper = oper
    allocate(node%left)
    allocate(node%right)
  case default
    call set_fct_error(type, "unknown type of node")
  endselect

endsubroutine new_fct_node

!------------------------------------------------------------------------------!
! delete_fct_node : remove FCT_NODE structure
!------------------------------------------------------------------------------!
recursive subroutine delete_fct_node(node) 
implicit none
! - parameters
type(st_fct_node) :: node

  select case(node%type_node)
  case(node_cst)
    call delete(node%container)
  case(node_var)
    call delete(node%container)
  case(node_opunit)
    call delete(node%left)
    deallocate(node%left)
  case(node_opbin)
    call delete(node%left)
    call delete(node%right)
    deallocate(node%left, node%right)
  case default
    call set_fct_error(node%type_node, "unknown type of node")
  endselect

endsubroutine delete_fct_node


endmodule FCT_NODE


!------------------------------------------------------------------------------!
! Changes history
!
! July  2004 : module creation
!------------------------------------------------------------------------------!
